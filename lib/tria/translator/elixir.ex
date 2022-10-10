defmodule Tria.Translator.Elixir do

  @moduledoc """
  Elixir to Tria translator.
  Expands macro
  Removes `&` captures
  Removes structures syntax
  Expands aliases, etc
  #TODO ignore `quote` in ast
  """

  @behaviour Tria.Translator

  import Tria.Common
  require Tria.Tri

  defmacro tri(ast) do
    quote do: Tria.Tri.tri([to_tria: false], unquote(ast))
  end

  # Public

  def to_tria!(ast, env) do
    {ast, _env} = expand_all(ast, env)
    ast
  rescue
    e ->
      inspect_ast(ast, label: :failed_translation)
      reraise e, __STACKTRACE__
  end

  def to_tria(ast, env) do
    {ast, env} = expand_all(ast, env)
    {:ok, ast, env}
  end

  # Because Tria is a subset of Elixir
  def from_tria(tria_ast) do
    Macro.prewalk(tria_ast, fn
      # Translates variables to counters
      {name, meta, context} when is_integer(context) ->
        {name, [{:counter, context} | meta], nil}

      # Makes structs from maps with :__struct__ key
      # {:"%{}", meta, items} = map->
      #   case Keyword.pop(items, :__struct__) do
      #     {nil, _} -> map
      #     {struct, items} -> {:%, meta, [struct, {:"%{}", meta, items}]}
      #   end

      other ->
        other
    end)
  end

  # Private

  # Expansion and walking the tree
  defp expand_all({:__aliases__, _, _} = aliased, env) do
    # Explicitly do this before expansion
    {unalias(aliased, env), env}
  end

  defp expand_all(ast, env) do
    # IO.inspect ast, label: :ast
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

      # Quoted
      # FIXME TODO handle opts
      {:quote, _meta, opts_body} ->
        {[do: body], _opts} = List.pop_at(opts_body, -1)
        {body, env} = expand_quote(body, env)
        {body, env}

      # Variable
      variable when is_elixir_variable(variable) ->
        variable = elixir_to_tria_variable(variable)
        {variable, add_variable(env, variable)}
        
      # List
      list when is_list(list) ->
        Enum.map_reduce(list, env, &expand_all/2)

      # Twople
      {l, r} ->
        {l, env} = expand_all(l, env)
        {r, env} = expand_all(r, env)
        {{l, r}, env}

      # Map Tuple
      {map_or_tuple, meta, items} when map_or_tuple in ~w[{} %{}]a ->
        {items, env} = expand_all(items, env)
        {{map_or_tuple, meta, items}, env}

      # Binary
      {:"<<>>", meta, items} ->
        # Binary syntax can have variables and macros inside
        # And operators (like `-`) are treated differently
        {items, env} =
          Enum.map_reduce(items, env, fn
            {:"::", meta, [left, right]}, env ->
              meta = ease(meta, env)
              {left, env} = expand_all(left, env)
              {right, env} =
                Macro.postwalk(right, env, fn
                  variable, env when is_elixir_variable(variable) ->
                    variable = elixir_to_tria_variable(variable)
                    {variable, add_variable(env, variable)}

                  other, env ->
                    {Macro.expand(other, env), env}
                end)
              { {:"::", meta, [left, right]}, env }

            other, env ->
              {Macro.expand(other, env), env}
          end)

        { {:"<<>>", meta, items}, env }
              

      # Structures
      tri %module{tri_splicing items} ->
        {items, env} = expand_all(items, env)
        q = quote do: %{unquote_splicing [{:__struct__, module} | items]}
        {q, env}

      # Closures
      {:&, meta, [{:/, slashmeta, [call, arity]}]} when is_integer(arity) ->
        {call, env} = expand_all(call, env)
        {{:&, meta, [{:/, slashmeta, [call, arity]}]}, env}

      {:"&", meta, [body]} ->
        {body, vars} =
          # Why only one prewalk?
          # Because nested captures are not allowed
          # And one prewalk can translate the whole structure
          prewalk(body, %{}, fn
            {:"&", _meta, [int]}, acc when is_integer(int) ->
              case acc do
                %{^int => variable} ->
                  {variable, acc}

                _ ->
                  variable = {:tria_capture, [counter: gen_uniq_context()], nil}
                  {variable, Map.put(acc, int, variable)}
              end

            other, acc ->
              {other, acc}
          end)

        vars =
          vars
          |> Enum.sort()
          |> Enum.map(fn {_, v} -> v end)

        expand_all({:fn, meta, [{:"->", [], [vars, body]}]}, env)

      # Fn
      {:fn, meta, clauses} ->
        {clauses, _internal_env} = expand_clauses(clauses, env)
        {{:fn, meta, clauses}, env}

      # Flow control primitives
      {:cond, meta, [[do: clauses]]} ->
        # cond clauses behave differently from case or fn clauses
        clauses =
          Enum.map(clauses, fn {:"->", meta, [[left], right]} ->
            {left, _} = expand_all(left, env)
            {right, _} = expand_all(right, env)
            {:"->", meta, [[left], right]}
          end)
        {{:cond, meta, [[do: clauses]]}, env}

      # Case
      {:case, meta, [arg, [do: clauses]]} ->
        {arg, env} = expand_all(arg, env)
        {clauses, _} = expand_clauses(clauses, env)
        {{:case, meta, [arg, [do: clauses]]}, env}

      # With
      {:with, meta, clauses} ->
        {clauses, [doelse]} = Enum.split(clauses, -1)
        doclause = Keyword.fetch!(doelse, :do)
        elseclause = Keyword.get(doelse, :else, [])

        {clauses, clauses_env} = expand_with_clauses(clauses, env)
        {doclause, _env} = expand_all(doclause, clauses_env)
        {elseclause, _internal_env} = expand_clauses(elseclause, env)

        case elseclause do
          e when e in [nil, []] ->
            {{:with, meta, clauses ++ [[do: doclause]]}, env}

          _ ->
            {{:with, meta, clauses ++ [[do: doclause, else: elseclause]]}, env}
        end

      # When
      {:when, meta, [pattern, guards]} ->
        {pattern, _env} = expand_all(pattern, %Macro.Env{env | context: :match})
        {guards, _env} = expand_all(guards, %Macro.Env{env | context: :guard})
        {{:when, meta, [pattern, guards]}, env}

      # Calls
      dot_call(aliased, function, args) ->
        module = unalias(aliased, env)
        {args, env} = expand_all(args, env)
        {dot_call(module, function, args), env}

      {calling_tuple, meta, args} when is_tuple(calling_tuple) ->
        {calling_tuple, env} = expand_all(calling_tuple, env)
        {args, env} = expand_all(args, env)
        {{calling_tuple, meta, args}, env}

      {:=, meta, [left, right]} ->
        {left, _env} = expand_all(left, %Macro.Env{env | context: :match})
        {right, env} = expand_all(right, env)
        {{:=, meta, [left, right]}, env}

      {name, meta, args} = ast when is_call(ast) ->
        arity = length(args)
        case special_form?(name, arity) do
          true ->
            {args, env} = expand_all(args, env)
            {{name, meta, args}, env}

          false ->
            case Macro.Env.lookup_import(env, {name, arity}) do
              [{:macro, module} | _] ->
                # We do not expand the args of macro, we just call the macro
                {Macro.expand(dot_call(module, name, args), env), env}

              [{:function, module} | _] ->
                case env do
                  # We're not in match or guard
                  %Macro.Env{context: nil} ->
                    {args, env} = expand_all(args, env)
                    {dot_call(module, name, args), env}

                  # We're in match or guard
                  _ ->
                    {args, env} = expand_all(args, env)
                    {{name, meta, args}, env}
                end

              [] ->
                case env do
                  # We're not in match or guard
                  %Macro.Env{context: nil} ->
                    #Local function maybe?
                    {args, env} = expand_all(args, env)
                    if defines?(env.module, name, length(args)) do
                      {dot_call(env.module, name, args), env}
                    else
                      {{name, [], args}, env}
                    end

                  # We're in match or guard
                  _ ->
                    {args, env} = expand_all(args, env)
                    {{name, meta, args}, env}
                end
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
    # Here we translate the metadata
    |> case do
      {{node, meta, children}, env} ->
        {{node, ease(meta, env), children}, env}

      other ->
        other
    end
  rescue
    e ->
      inspect_ast(ast, label: :failed, with_contexts: true)
      reraise e, __STACKTRACE__
  end

  # Expands body of `quote`

  defp expand_quote([{:unquote_splicing, _, [unquoted]} | tail], env) do
    {unquoted, env} = expand_all(unquoted, env)
    {tail, env} = expand_quote(tail, env)
    case unquoted do
      list when is_list(list) ->
        { list ++ tail, env }

      _unquoted ->
        raise "Not implemented"
    end
  end

  defp expand_quote({:unquote, _, [unquoted]}, env) do
    expand_all(unquoted, env)
    |> tap(fn {x, _} -> IO.inspect(x, label: :unquoted) end)
  end

  defp expand_quote({left, right}, env) do
    {left, env} = expand_quote(left, env)
    {right, env} = expand_quote(right, env)
    { {left, right}, env }
  end

  defp expand_quote([head | tail], env) do
    {head, env} = expand_quote(head, env)
    {tail, env} = expand_quote(tail, env)
    { [head | tail], env }
  end

  defp expand_quote({operation, meta, args}, env) do
    {operation, env} = expand_quote(operation, env)
    {args, env} = expand_quote(args, env)
    
    { {:{}, [], [operation, meta, args]}, env }
  end

  defp expand_quote(literal, env) do
    {literal, env}
  end

  # With clauses are inherited
  defp expand_with_clauses(clauses, env) do
    Enum.map_reduce(clauses, env, fn
      {:"<-", meta, [left, right]}, env ->
        [left] = expand_clause_args([left], env)
        {right, env} = expand_all(right, env)
        {{:"<-", meta, [left, right]}, env}

      other, env ->
        #FIXME not sure about this, needs testing
        # I mean, I am not sure that env inheritance works this way
        # inside `with`. But who the fuck knows, right?
        expand_all(other, env)
    end)
  end

  # Case, fn and cond clauses have their own contexts
  defp expand_clauses(clauses, env) do
    Enum.map_reduce(clauses, env, fn
      {:"->", meta, [args, body]}, env ->
        args = expand_clause_args(args, env)
        {body, _internal_env} = expand_all(body, env)
        {{:"->", meta, [args, body]}, env}
    end)
  end

  defp expand_clause_args([{:when, meta, args_and_guards}], env) do
    {guards, args} = List.pop_at(args_and_guards, -1)
    {guards, _internal_env} = expand_all(guards, %Macro.Env{env | context: :guard})
    {args,   _internal_env} = expand_all(args,   %Macro.Env{env | context: :match})
    [{:when, meta, args ++ [guards]}]
  end
  defp expand_clause_args(args, env) do
    {args, _env} = expand_all(args, %Macro.Env{env | context: :match})
    args
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
  defp unalias(other, _env), do: other

  defp normalize_alias_to({:__aliases__, _, modules}), do: Module.concat(modules)
  defp normalize_alias_to(module), do: Module.concat([module])

  defp special_form?(:".", _), do: true
  defp special_form?(op, arity) when is_integer(arity), do: Macro.special_form?(op, arity)
  # defp special_form?(op, args) when is_list(args), do: Macro.special_form?(op, length(args))

  # Checks if this function is defined anywhere
  defp defines?(module, function, arity) do
    if Code.ensure_loaded?(module) do
      function_exported?(module, function, arity) or !!Tria.Analyzer.fetch_abstract({module, function, arity})
    else
      Enum.any?(~w[def defp defmacro defmacrop]a, &Module.defines?(module, {function, arity}, &1))
    end
  rescue
    ArgumentError -> false
  end

  # Meta Triafication

  # When
  defp ease(meta, %Macro.Env{file: file}) do
    case meta[:keep] do
      {file, line} ->
        meta
        |> Keyword.put(:line, line)
        |> Keyword.put(:file, file)

      _ ->
        meta
        |> Keyword.put(:file, file)
    end
    |> Keyword.take(~w[file line generated ambiguous_op var]a)
  end

  defp elixir_to_tria_variable({name, meta, _context} = variable) do
    case meta[:counter] do
      # FIXME, needs proper translation, because I am not sure that these
      # intergers are unique in all contexts
      {_context, integer} ->
        {name, meta, integer}

      nil ->
        variable

      integer when is_integer(integer) ->
        {name, meta, integer}
    end
  end

  #FIXUP twople represented as tuple
  # defp {:"{}", _meta, [left, ight]}), do: {left, right}
  # defp maybe_twople(other), do: other

end
