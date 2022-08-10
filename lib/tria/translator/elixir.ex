defmodule Tria.Translator.Elixir do

  @moduledoc """
  Elixir to Tria translator
  #TODO ignore `quote` in ast
  """

  @behaviour Tria.Translator

  import Tria.Common
  import Tria.Tri

  def to_tria!(ast, env) do
    {ast, _env} = expand_all(ast, env)
    ast
  end

  def to_tria(ast, env) do
    {ast, env} = expand_all(ast, env)
    {:ok, ast, env}
  end

  # Because Tria is a subset of Elixir
  def from_tria(tria_ast), do: tria_ast

  # Expansion and walking the tree
  defp expand_all({:__aliases__, _, _} = aliased, env) do
    # Explicitly do this before expansion
    {unalias(aliased, env), env}
  end

  defp expand_all(ast, env) do
    case Macro.expand(ast, env) do
      # Imports, aliases, requires and other stuff which changes env
      tri(require something, as: an_alias) ->
        module = unalias(something, env)
        env =
          env
          |> add_require(module)
          |> add_alias(module, an_alias)
        {module, env}

      tri require something ->
        module = unalias(something, env)
        {module, add_require(env, module)}

      tri(alias something, as: an_alias) ->
        module = unalias(something, env)
        {module, add_alias(env, module, an_alias)}

      tri alias something ->
        case something do
          {{:".", _, [base, :"{}"]}, _, tails} ->
            base = unalias(base, env)
            modules = Enum.map(tails, &Module.concat(base, unalias(&1, env)))
            env =
              Enum.reduce(modules, env, fn module, env ->
                alias_to = List.last Module.split module
                add_alias(env, module, alias_to)
              end)
            {modules, env}

          module ->
            module = unalias(module, env)
            alias_to = List.last Module.split module
            {module, add_alias(env, module, alias_to)}
        end

      tri import something ->
        module = unalias(something, env)
        {module, add_import(env, module)}


      tri(import something, opts) ->
        module = unalias(something, env)
        {module, add_import(env, module, opts)}

      # Variable
      {name, meta, ctx} = variable when is_variable(variable) ->
        variable =
          if counter = meta[:counter] do
            {name, [], :"#{ctx}#{counter}"}
          else
            {name, [], ctx}
          end
        {variable, add_variable(env, variable)}
        
      # Collections
      list when is_list(list) ->
        reverse Enum.reduce(list, {[], env}, fn item, {items, env} ->
          {item, env} = expand_all(item, env)
          {[item | items], env}
        end)

      {l, r} ->
        {l, env} = expand_all(l, env)
        {r, env} = expand_all(r, env)
        {{l, r}, env}

      {map_or_tuple, meta, items} when map_or_tuple in ~w[{} %{}]a ->
        {items, env} = expand_all(items, env)
        {{map_or_tuple, meta, items}, env}

      # Closures
      {:"&", meta, body} ->
        ctx = gen_uniq_context()
        {body, vars} =
          Macro.prewalk(body, [], fn
            {:"&", _meta, [int]}, acc ->
              v = {:"x#{int}", [], ctx}
              {v, [v | acc]}

            other, acc ->
              {other, acc}
          end)

        vars = Enum.sort vars
        expand_all({:fn, meta, [{:"->", [], [vars, body]}]}, env)
        
      {:fn, meta, clauses} ->
        {clauses, _internal_env} = expand_clauses(clauses, env)
        {{:fn, meta, clauses}, env}

      # Flow control primitives
      {:cond, meta, [[do: clauses]]} ->
        {clauses, _} = expand_clauses(clauses, env)
        {{:cond, meta, [[do: clauses]]}, env}

      {:case, meta, [arg, [do: clauses]]} ->
        {arg, env} = expand_all(arg, env)
        {clauses, _} = expand_clauses(clauses, env)
        {{:case, meta, [arg, [do: clauses]]}, env}

      {:with, meta, clauses} ->
        {clauses, [doelse]} = Enum.split(clauses, -1)
        elseclause = Keyword.get(doelse, :else, [])
        doclause = Keyword.fetch!(doelse, :do)

        {clauses, clauses_env} = expand_with_clauses(clauses, env)
        {doclause, _env} = expand_all(doclause, clauses_env)
        {elseclause, _internal_env} = expand_clauses(elseclause, env)

        {{:with, meta, clauses ++ [[do: doclause, else: elseclause]]}, env}

      # Calls
      dot_call(aliased, function, args) ->
        module = unalias(aliased, env)
        {args, env} = expand_all(args, env)
        {dot_call(module, function, args), env}

      {calling_tuple, meta, args} when is_tuple(calling_tuple) ->
        {calling_tuple, env} = expand_all(calling_tuple, env)
        {args, env} = expand_all(args, env)
        {{calling_tuple, meta, args}, env}

      {name, meta, args} = ast when is_call(ast) ->
        arity = length(args)
        case Macro.special_form?(name, arity) do
          true ->
            {args, env} = expand_all(args, env)
            {{name, meta, args}, env}

          false ->
            case Macro.Env.lookup_import(env, {name, arity}) do
              [{:macro, module} | _] ->
                # We do not expand the args of macro, we just call the macro
                {Macro.expand(dot_call(module, name, args), env), env}

              [{:function, module} | _] ->
                {args, env} = expand_all(args, env)
                {dot_call(module, name, args), env}

              [] ->
                #Local function maybe?
                {args, env} = expand_all(args, env)
                {{name, meta, args}, env}
            end
        end

      # Literal
      literal ->
        case Macro.quoted_literal?(literal) do
          true ->
            {literal, env}

          false ->
            IO.inspect literal, pretty: true, label: :not_implemented
            raise "Not implemented"
        end
    end
  end

  # With clauses are inherited
  defp expand_with_clauses(clauses, env) do
    reverse Enum.reduce(clauses, {[], env}, fn
      {:"<-", meta, [left, right]}, {clauses, env} ->
        {left, _internal_env} = expand_all(left, env)
        {right, env} = expand_all(right, env)
        {[{:"<-", meta, [left, right]} | clauses], env}
    end)
  end

  # Case, fn and cond clauses have their own contexts
  defp expand_clauses(clauses, env) do
    reverse Enum.reduce(clauses, {[], env}, fn
      {:"->", meta, [args, body]}, {clauses, env} ->
        {args, _internal_env} = expand_all(args, env)
        {body, _internal_env} = expand_all(body, env)
        {[{:"->", meta, [args, body]} | clauses], env}
    end)
  end

  # Macro.Env helpers

  defp add_require(%Macro.Env{requires: requires} = env, module) do
    %Macro.Env{env | requires: [module | requires]}
  end

  defp add_alias(%Macro.Env{aliases: aliases} = env, module, alias_to) do
    alias_to = normalize_alias_to alias_to
    %Macro.Env{env | aliases: [{alias_to, module} | aliases]}
  end

  defp add_import(%Macro.Env{functions: env_functions} = env, module, opts \\ []) do
    functions = fn -> module.__info__(:functions) end
    macros    = fn -> module.__info__(:macros) end

    only =
      case Keyword.fetch(opts, :only) do
        :error -> functions.() ++ macros.()
        {:ok, :functions} -> functions.()
        {:ok, :macros} -> macros.()
        {:ok, other} -> other #TODO check for non-existing functions
      end
    except = Keyword.get(opts, :except, [])

    imported = only -- except

    %Macro.Env{env | functions: Keyword.put(env_functions, module, imported)}
  end

  defp add_variable(%Macro.Env{versioned_vars: versioned_vars} = env, {name, meta, context}) do
    context =
      case Keyword.fetch(meta, :counter) do
        {:ok, integer} -> integer
        :error -> context
      end

    max_version =
      versioned_vars
      |> Map.values()
      |> Enum.max(fn -> 0 end)

    versioned_vars = Map.put(versioned_vars, {name, context}, max_version + 1)

    %Macro.Env{env | versioned_vars: versioned_vars}
  end

  defp unalias(module, _env) when is_atom(module), do: module
  defp unalias({:__aliases__, _, [module | tail]} = aliased, %{aliases: aliases} = env) do
    case Keyword.fetch(aliases, Module.concat([module])) do
      {:ok, found} ->
        Module.concat([found | tail])

      :error ->
        case Macro.expand(aliased, env) do
          module when is_atom(module) ->
            module

          other ->
            unalias(other)
        end
    end
  end

  defp reverse({list, env}), do: {:lists.reverse(list), env}

  defp normalize_alias_to({:__aliases__, _, modules}), do: Module.concat(modules)
  defp normalize_alias_to(module), do: Module.concat([module])

end
