defmodule Tria.Compiler.ElixirTranslator do

  @moduledoc """
  # Elixir to Tria translator.

  ## Tria language

  Tria is designed to simplify AST manipulations in Elixir.

  Tria is different from Elixir in these things:
  * No macros and `quote`-s
  * No `&` captures
  * No aliases
  * No local calls
  * No `rescue` in `try`
  * `after` in `receive` is twople instead of right arrow
  * No `cond`
  * Left part of `->`, `=`, `<-` is always a pattern!

  These changes add some rules and remove some exceptions to make
  reasoning about language simpler. For example, in `left -> right` left is always pattern and right is always body
  """

  @behaviour Tria.Compiler.Translator

  import Tria.Debug.Breakpoint, warn: false

  import Tria.Language
  import Tria.Language.Tri
  import Tria.Language.Meta

  alias Tria.Debug
  alias Tria.Language.Codebase
  alias Tria.Language.Guard

  @tri_opts [to_tria: false]

  # Public

  @doc """
  Translates Elixir AST to Tria AST, raising
  """
  @impl true
  def to_tria!(ast, env \\ empty_env(nil)) do
    {:ok, ast, _env} = to_tria(ast, env)
    ast
  rescue
    e ->
      inspect_ast(ast, label: :failed_translation)
      reraise e, __STACKTRACE__
  end

  @doc """
  Translates Elixir AST to Tria AST, non-raising
  """
  @impl true
  def to_tria(ast, env \\ empty_env(nil)) do
    env =
      env
      |> Map.update!(:functions, &Keyword.delete(&1, env.module))
      |> Map.update!(:macros, &Keyword.delete(&1, env.module))

    {ast, env} = expand_all(ast, env)
    {:ok, ast, env}
  end

  @doc """
  Translates Tria AST to Elixir AST
  """
  @impl true
  def from_tria(tria_ast, opts \\ []) do
    # Because Tria is a subset of Elixir
    prewalk(tria_ast, fn
      # Translates tria variables to elixir variables
      {name, meta, counter} when is_integer(counter) ->
        {name, [{:counter, counter} | meta], nil}

      # Translate fn to &capture/arity
      {:fn, _, [{:"->", _, [args1, dot_call(module, function, args2)]}]}  = the_fn ->
        if unmeta(args1) == unmeta(args2) do
          quote do: &unquote(module).unquote(function)/unquote(length args1)
        else
          the_fn
        end

      {:try, meta, [clauses]} ->
        clauses = unless opts[:leave_try], do: try_to_elixir(clauses), else: clauses
        {:try, meta, [clauses]}

      {:receive, meta, [[do: body, after: {timeout, after_body}]]} ->
        {:receive, meta, [[do: body, after: [{:"->", [], [[timeout], after_body]}]]]}

      other ->
        other
    end)
  end

  @doc """
  Returns full module atom from an alias
  """
  @spec unalias(Macro.t()) :: module() | Macro.t()
  def unalias({:__aliases__, meta, names} = ast) when is_aliases(ast) do
    if the_alias = meta[:alias], do: the_alias, else: Module.concat(names)
  end
  def unalias(other), do: other

  # Private

  # Expansion and walking the tree
  defp expand_all(breakpoint(point), env) do
    handle_breakpoint(point)
    { nil, env }
  end

  # Explicitly do this before expansion
  defp expand_all({:__aliases__, _, _} = aliased, env) do
    { unalias(aliased, env), env }
  end

  # __ENV__ is ****ing special because it has non-ast forms inside
  defp expand_all({:__ENV__, _, _}, env) do
    { Macro.escape(env), env }
  end

  # Defmodule and other defs
  defp expand_all({:defmodule, meta, [name, [do: body]]}, env) do
    inner_env = add_module(env, name)
    { body, _ } = expand_all(body, inner_env)
    { {:defmodule, meta, [name, [do: body]]}, env }
  end

  defp expand_all({kind, meta, [{name, smeta, args}, body]}, env) when kind in ~w[def defp defmacro defmacrop]a do
    { args, _} = expand_all(args, env)
    inner_env = add_function(env, name, args)
    { body, _ } = expand_all(body, inner_env)
    { {kind, meta, [{name, smeta, args}, body]}, env }
  end

  defp expand_all(ast, env) do
    case Macro.expand(ast, env) do
      # Imports, aliases, requires and other stuff which changes env
      {:__aliases__, _, _} = aliased ->
        { unalias(aliased, env), env }

      tri(require something, as: an_alias) ->
        module = unalias(something, env)
        env =
          env
          |> add_require(module)
          |> add_alias(module, an_alias)
        { module, env }

      tri require something ->
        module = unalias(something, env)
        { module, add_require(env, module) }

      tri(alias something, as: an_alias) ->
        module = unalias(something, env)
        { module, add_alias(env, module, an_alias) }

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
            { modules, env }

          module ->
            module = unalias(module, env)
            alias_to = List.last Module.split module
            { module, add_alias(env, module, alias_to) }
        end

      tri import something ->
        module = unalias(something, env)
        { module, add_import(env, module) }

      tri(import something, opts) ->
        module = unalias(something, env)
        { module, add_import(env, module, opts) }

      # Quoted
      # FIXME TODO handle opts
      {:quote, _meta, opts_body} when is_list(opts_body) ->
        expand_quote(opts_body, env)
        # |> tap(fn {body, _} -> IO.inspect(body, label: :expanded) end)

      # Variable
      variable when is_elixir_variable(variable) ->
        variable = elixir_to_tria_variable(variable)
        { variable, add_variable(env, variable) }

      # List
      list when is_list(list) ->
        Enum.map_reduce(list, env, &expand_all/2)

      # Twople
      {l, r} ->
        {l, env} = expand_all(l, env)
        {r, env} = expand_all(r, env)
        { {l, r}, env }

      # Map Tuple
      {map_or_tuple, meta, items} when map_or_tuple in ~w[{} %{}]a ->
        {items, env} = expand_all(items, env)
        { {map_or_tuple, meta, items}, env }

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
              expand_all(other, env)
          end)

        { {:"<<>>", meta, items}, env }


      # Structures
      tri %name{tri_splicing items} ->
        {items, env} = expand_all(items, env)
        {name, _} = expand_all(name, env)

        # if is_atom(name) and env.context == nil do
        #   { quote(do: unquote(name).__struct__(unquote(items))), env }
        # else
        #   { quote(do: %unquote(name){unquote_splicing items}), env }
        # end
        { quote(do: %unquote(name){unquote_splicing items}), env }

      # Closures
      {:&, meta, [{:/, slashmeta, [call, arity]}]} when is_integer(arity) ->
        {call, env} =
          case call do
            {name, meta, context} when is_variable(name, meta, context) ->
              case lookup_call(name, arity, env) do
                {_, module} ->
                  { {:".", ease(meta, env), [module, name]}, env }

                _ ->
                  { name, env }
              end

            dot_call(module, func, []) ->
              module = unalias(module, env)
              expand_all({:".", [], [module, func]}, env)
          end

        args =
          nil
          |> List.duplicate(arity)
          # Here we put this context meta because it will be expanded later
          |> Enum.map(fn _ -> {:tria_capture, [counter: gen_uniq_context()], nil} end)

        body = {call, ease(slashmeta, env), args}

        expand_all({:fn, meta, [{:"->", meta, [args, body]}]}, env)

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
                  # Here we put this context meta because it will be expanded later
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

        expand_all({:fn, meta, [{:"->", meta, [vars, body]}]}, env)

      # Fn
      {:fn, meta, clauses} when is_list(clauses) ->
        {clauses, _internal_env} = expand_clauses(clauses, env)
        {{:fn, meta, clauses}, env}

      # Flow control primitives
      # Cond
      {:cond, meta, [[do: clauses]]} ->
        # cond clauses behave differently from case or fn clauses
        clauses =
          Enum.map(clauses, fn {:"->", meta, [[left], right]} ->
            {left, _} = expand_all(left, env)
            {right, _} = expand_all(right, env)
            {:"->", meta, [[left], right]}
          end)
        {{:cond, meta, [[do: clauses]]}, env}

      # Try
      {:try, meta, [parts]} ->
        parts =
          Enum.map(parts, fn
            {do_or_after, body} when do_or_after in ~w[do after]a ->
              {body, _} = expand_all(body, env)
              {do_or_after, body}

            {rescue_or_catch, clauses} when rescue_or_catch in ~w[rescue catch]a ->
              {clauses, _} = expand_rescue_clauses(clauses, env)
              {rescue_or_catch, clauses}

            {:else, clauses} ->
              {clauses, _} = expand_clauses(clauses, env)
              {:else, clauses}
          end)

        { {:try, meta, [parts]}, env }

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
      {:when, meta, [left, right]} ->
        # left_context = env.context && :guard || :match
        # {left, _env} = expand_all(left, %Macro.Env{env | context: left_context})
        {left, _env}  = expand_all(left,  env)
        {right, _env} = expand_all(right, env)
        {{:when, meta, [left, right]}, env}

      # Receive
      {:receive, meta, [[do: body, after: [{:"->", _, [[timeout], after_body]}]]]} ->
        {body, _env} = expand_all(body, env)
        {timeout, env} = expand_all(timeout, env)
        {after_body, env} = expand_all(after_body, env)
        {{:receive, meta, [[do: body, after: {timeout, after_body}]]}, env}

      # Calls
      dot_call(subject, function, args) ->
        {subject, _env} = expand_all(subject, env)
        subject = unalias(subject, env)
        {args, env} = expand_all(args, env)
        {dot_call(subject, function, args), env}

      {calling_tuple, meta, args} when is_tuple(calling_tuple) ->
        {calling_tuple, env} = expand_all(calling_tuple, env)
        {args, env} = expand_all(args, env)
        {{calling_tuple, meta, args}, env}

      {:=, meta, [left, right]} ->
        {left, _env} = expand_all(left, %Macro.Env{env | context: :match})
        {right, env} = expand_all(right, env)
        {{:=, meta, [left, right]}, env}

      {{:".", dotmeta, [left, right]}, meta, []} when is_atom(right) ->
        {left, env} = expand_all(left, env)
        {right, env} = expand_all(right, env)
        quoted =
          if meta[:no_parens] do
            raise "Not implemented"
          else
            {{".", ease(dotmeta, env), [left, right]}, meta, []}
          end

        { quoted, env }

      {name, meta, args} = ast when is_call(ast) ->
        case lookup_call(name, length(args), env) do
          :special_form ->
            {args, env} = expand_all(args, env)
            {{name, meta, args}, env}

          {:macro, module} ->
            # We do not expand the args of macro, we just call the macro
            {Macro.expand(dot_call(module, name, args), env), env}

          {:function, Kernel} ->
            {args, env} = expand_all(args, env)
            {dot_call(Kernel, name, args), env}

          {:function, module} ->
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

          nil ->
            case env do
              # We're not in match or guard
              %Macro.Env{context: nil} ->
                #Local function maybe?
                {args, env} = expand_all(args, env)
                if defines?(env.module, name, length(args)) do
                  {dot_call(env.module, name, args), env}
                else
                  {{name, meta, args}, env}
                end

              # We're in match or guard
              _ ->
                {args, env} = expand_all(args, env)
                {{name, meta, args}, env}
            end
        end

      # __CALLER__ and __STACKTRACE__ are the only variables
      # which are not expanded by the Macro.expand
      special_var when is_special_variable(special_var) ->
        {special_var, env}

      # Literal
      literal ->
        case Macro.quoted_literal?(literal) do
          true ->
            {literal, env}


          false ->
            Debug.inspect literal, pretty: true, label: :not_implemented
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
      inspect_ast(ast, label: :failed_to_translate, with_contexts: true)
      reraise e, __STACKTRACE__
  end

  ## Quote

  defp expand_quote(opts_body, env) do
    {body, opts} = split_quote_opts(opts_body)
    opts = Map.new opts

    ex_opts =
      opts
      |> Map.put_new(:unquote, if(opts[:bind_quoted], do: false, else: true))
      |> Map.put_new(:context, env.context || env.module)

    {body, env} = expand_quote(body, env, ex_opts)
    body =
      body
      |> prepend_bind_quoted(ex_opts)
      |> prepend_validations(opts) # We prepend validations only for original opts

    {body, env}
  end

  defp split_quote_opts([[do: body]]), do: {body, []}
  defp split_quote_opts([opts, [do: body]]), do: {body, opts}
  defp split_quote_opts([opts_body]), do: Keyword.pop!(opts_body, :do)

  defp prepend_validations(body, opts) do
    validations =
      for {key, value} when key in ~w[line file context generated unquote]a <- opts do
        quote do: :elixir_quote.validate_runtime(unquote(key), unquote(value))
      end

    case validations do
      [] -> body
      validations -> {:__block__, [], validations ++ [body]}
    end
  end

  defp prepend_bind_quoted(body, %{bind_quoted: [_ | _] = binds, context: context}) do
    escaped_binds =
      for {name, code} <- binds do
        {:{}, [], [:=, [], [{:{}, [], [name, [], context]}, code]]}
      end
    {:{}, [], [:__block__, [], escaped_binds ++ [body]]}
  end
  defp prepend_bind_quoted(body, _), do: body

  # Actual tree expansion happens here

  # Unquote Splicing
  defp expand_quote([{:unquote_splicing, _, [unquoted]} | tail], env, %{unquote: true} = opts) do
    {unquoted, env} = expand_all(unquoted, env)
    {tail, env} = expand_quote(tail, env, opts)
    { dot_call(:elixir_quote, :list, [unquoted, tail]), env }
  end

  # Unquote
  defp expand_quote({:unquote, _, [unquoted]}, env, %{unquote: true}) do
    expand_all(unquoted, env)
  end

  # unquote in dotcall with args
  defp expand_quote(
    {{{:., _, [left, :unquote]}, _, [right]}, meta, args},
    env,
    %{unquote: true, context: context} = opts
  ) do
    {left, env} = expand_quote(left, env, opts)
    {right, env} = expand_all(right, env)
    {args, env} = expand_quote(args, env, opts)

    { dot_call(:elixir_quote, :dot, [meta, left, right, args, context]), env }
  end

  # unquote in dotcall without args or parens
  defp expand_quote(
    {{:., _, [left, :unquote]}, meta, [right]},
    env,
    %{unquote: true, context: context} = opts
  ) do
    meta = expand_quote_meta(meta, env, opts)

    {left, env} = expand_quote(left, env, opts)
    {right, env} = expand_all(right, env)
    { dot_call(:elixir_quote, :dot, [meta, left, right, nil, context]), env }
  end

  # unquote(something)() call
  defp expand_quote({{:unquote, _meta, [something]}, meta, args}, env, %{unquote: true} = opts) do
    meta = expand_quote_meta(meta, env, opts)
    {something, env} = expand_all(something, env)
    {args, env} = expand_quote(args, env, opts)

    { {:{}, [], [something, meta, args]}, env }
  end

  # quote in quote
  defp expand_quote({:quote, _, opts_body}, env, opts) do
    {opts_body, env} = expand_quote(opts_body, env, %{opts | unquote: false})
    { {:{}, [], [:quote, [], opts_body]}, env }
  end

  # Modules Aliases are isolated
  # Not sure about this
  # defp expand_quote({:__aliases__, _, _} = aliases, env, _opts) do
  #   expand_all(aliases, env)
  # end

  # Twople
  defp expand_quote({left, right}, env, opts) do
    {left, env} = expand_quote(left, env, opts)
    {right, env} = expand_quote(right, env, opts)
    { {left, right}, env }
  end

  # List
  defp expand_quote([head | tail], env, opts) do
    {head, env} = expand_quote(head, env, opts)
    {tail, env} = expand_quote(tail, env, opts)
    { [head | tail], env }
  end

  # Variable
  defp expand_quote({name, meta, ctx}, env, %{context: new_ctx} = opts) when is_atom(ctx) do
    meta = expand_quote_meta([{:if_undefined, :apply} | meta], env, opts)
    { {:{}, [], [name, meta, new_ctx]}, env }
  end

  # Call
  defp expand_quote({operation, meta, args}, env, opts) when is_atom(operation) and is_list(args) do
    meta = expand_quote_meta(meta, env, opts)

    {args, env} = expand_quote(args, env, opts)
    meta =
      with(
        true <- is_list(args) and not List.improper?(args),
        {_, module} <- lookup_call(operation, length(args), env)
      ) do
        opts.context
        && [{:context, opts.context}, {:import, module} | meta]
        || [{:import, module} | meta]
      else
        _ -> meta
      end

    { {:{}, [], [operation, meta, args]}, env }
  end

  # Triple
  defp expand_quote({operation, meta, args}, env, opts) do
    {operation, env} = expand_quote(operation, env, opts)
    {args, env} = expand_quote(args, env, opts)

    { {:{}, [], [operation, expand_quote_meta(meta, env, opts), args]}, env }
  end

  # List
  defp expand_quote(literal, env, _) do
    {literal, env}
  end

  # Meta
  defp expand_quote_meta(meta, env, %{location: :keep}) do
    keep = {Path.relative_to_cwd(env.file), env.line}
    [{:keep, keep} | meta]
  end

  defp expand_quote_meta(meta, env, opts) do
    meta
    |> add_meta_env_option(env, opts, :file)
    |> add_meta_env_option(env, opts, :line)
    |> add_meta_generated(opts)
  end

  defp add_meta_env_option(meta, env, opts, key) do
    case opts do
      %{^key => true} -> [{key, Map.fetch!(env, key)} | meta]
      %{^key => value} when value != false -> [{key, value} | meta]
      _ -> meta
    end
  end

  defp add_meta_generated(meta, %{generated: generated}) do
    [{:generated, generated} | meta]
  end
  defp add_meta_generated(meta, _), do: meta

  ## With clauses

  defp expand_with_clauses(clauses, env) do
    Enum.map_reduce(clauses, env, fn
      {:"<-", meta, [left, right]}, env ->
        [left] = expand_clause_args([left], env)
        {right, env} = expand_all(right, env)
        {{:"<-", meta, [left, right]}, env}

      other, env ->
        #FIXME not sure about this, needs testing
        # I mean, I am not sure that env inheritance works this way
        # inside `with`. But who the **** knows, right?
        expand_all(other, env)
    end)
  end

  # Case, fn and cond clauses have their own contexts
  defp expand_rescue_clauses(clauses, env) do
    Enum.map_reduce(clauses, env, fn
      {:"->", meta, [[{:in, inmeta, [arg, exception]}], body]}, env ->
        [arg] = expand_clause_args([arg], env)
        {body, _internal_env} = expand_all(body, env)
        {{:"->", meta, [[{:in, inmeta, [arg, exception]}], body]}, env}

      {:"->", meta, [args, body]}, env ->
        args = expand_clause_args(args, env)
        {body, _internal_env} = expand_all(body, env)
        {{:"->", meta, [args, body]}, env}
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
    {args,   _internal_env} = expand_all(args,   %Macro.Env{env | context: :match})
    {guards, _internal_env} = expand_all(guards, %Macro.Env{env | context: :guard})
    [{:when, meta, args ++ [guards]}]
  end
  defp expand_clause_args(args, env) do
    {args, _env} = expand_all(args, %Macro.Env{env | context: :match})
    args
  end

  defp lookup_call(name, arity, env) do
    case special_form?(name, arity) do
      true  -> :special_form
      false -> List.first Macro.Env.lookup_import(env, {name, arity})
    end
  end

  # Macro.Env helpers

  defp add_module(%Macro.Env{module: current_module} = env, aliased) do
    module = unalias(aliased, env)
    %Macro.Env{env | module: Module.concat(current_module, module)}
  end

  defp add_function(%Macro.Env{function: nil} = env, name, args) when is_list(args) do
    %Macro.Env{env | function: {name, length(args)}}
  end

  defp add_function(%Macro.Env{function: nil} = env, name, atom) when is_atom(atom) do
    %Macro.Env{env | function: {name, 0}}
  end

  defp add_require(%Macro.Env{requires: requires} = env, module) do
    %Macro.Env{env | requires: [module | requires]}
  end

  defp add_alias(%Macro.Env{aliases: aliases} = env, module, alias_to) do
    alias_to = normalize_alias_to alias_to
    %Macro.Env{env | aliases: [{alias_to, module} | aliases]}
  end

  defp add_import(%Macro.Env{functions: env_functions, macros: env_macros} = env, module, opts \\ []) do
    functions = fn -> module.__info__(:functions) end
    macros    = fn -> module.__info__(:macros) end

    {fonly, monly} =
      case Keyword.fetch(opts, :only) do
        :error -> {functions.(), macros.()}
        {:ok, :functions} -> {functions.(), []}
        {:ok, :macros} -> {[], macros.()}
        {:ok, other} -> Enum.split_with(other, fn x -> x in functions.() end)
      end

    {fexcept, mexcept} =
      opts
      |> Keyword.get(:except, [])
      |> Enum.split_with(fn x -> x in functions.() end)

    mimported = Enum.sort(monly -- mexcept)
    fimported = Enum.sort(fonly -- fexcept)

    %Macro.Env{env | functions: Keyword.put(env_functions, module, fimported), macros: Keyword.put(env_macros, module, mimported)}
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

  # Here I don't write Macro.Env because sometime I use field-polymorphic structure
  defp unalias(module, _env) when is_atom(module), do: module
  defp unalias({:__aliases__, _, [{:__MODULE__, _, _} | tail]}, %{module: module} = env) do
    unalias({:__aliases__, [], [module | tail]}, env)
  end
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
      function_exported?(module, function, arity) or !!Codebase.fetch_abstract({module, function, arity})
    else
      Enum.any?(~w[def defp defmacro defmacrop]a, &Module.defines?(module, {function, arity}, &1))
    end
  rescue
    ArgumentError -> false
  end

  # Meta Triafication

  # When
  defp ease(meta, %Macro.Env{file: file}) do
    file = Path.relative_to_cwd(file)
    case meta[:keep] do
      {file, line} ->
        meta
        |> Keyword.put(:line, line)
        |> Keyword.put(:file, Path.relative_to_cwd file)

      _ ->
        case meta[:line] do
          nil -> Keyword.put(meta, :keep, file)
          line -> Keyword.put(meta, :keep, {file, line})
        end
    end
    |> Keyword.put_new(:file, file)
    |> Keyword.take(~w[file line keep generated ambiguous_op var]a)
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

  ### Tria to Elixir translation

  defp try_to_elixir(clauses) do
    Enum.flat_map(clauses, fn
      {:catch, clauses} -> catch_to_elixir(clauses)
      other -> [other]
    end)
  end

  defp catch_to_elixir(clauses) when is_list(clauses) do
    {catch_clauses, rescue_clauses} =
      clauses
      |> Enum.map(fn {:"->", meta, [pattern, body]} ->
        # inspect_ast(pattern, label: :pattern)
        {guards, pattern} = Guard.pop_guard(pattern)
        case pattern do
          # Case where exception is an erlang exception wrapped into elixir's exception structure
          # Just like erlang's `undef` means the same as Elixir's `UndefinedFunctionError`
          [:error, exception] when is_variable(exception) ->
            case extract_exceptions guards do
              # Clause for `any -> body` style exceptions
              [] ->
                body = denormalize_catch_body(body, exception)
                {nil, {:"->", meta, [[exception], body]}}

              exceptions ->
                rescue_pattern = [quote do: unquote(exception) in unquote(exceptions)]
                body = denormalize_catch_body(body, exception)
                {nil, {:"->", meta, [rescue_pattern, body]}}
            end

          [:throw, pattern] ->
            {{:"->", meta, [Guard.append_guard([pattern], guards), body]}, nil}

          [kind, pattern] ->
            {{:"->", meta, [Guard.append_guard([kind, pattern], guards), body]}, nil}

          _ ->
            {{:"->", meta, [Guard.append_guard([pattern], guards), body]}, nil}
        end

      end)
      |> Enum.unzip()

    catch_clauses = Enum.reject(catch_clauses, &is_nil/1)
    rescue_clauses = Enum.reject(rescue_clauses, &is_nil/1)

    Enum.reject([rescue: rescue_clauses, catch: catch_clauses], &match?({_, []}, &1))
  end

  defp denormalize_catch_body(body, exception) do
    case body do
      # First clause, where erlang's exception is normalized
      (tri do
        x = tri dot_call(Exception, :normalize, _)
        tri_splicing block
      end) ->
        {:__block__, [], [tri(x = exception) | block]}

      # Second clause where we don't use exception in `rescue`, therefore
      # there is no normalisation
      other ->
        other
    end
  end

  defp extract_exceptions(tri left when right) do
    # This concatenation is okay here, because it is not deep
    extract_exceptions(left) ++ extract_exceptions(right)
  end

  defp extract_exceptions(ast) do
    case ast do
      {{:., _, [:erlang, :andalso]}, _, [
        {{:., _, [Kernel, :==]}, _, [
          {{:., _, [:erlang, :map_get]}, _, [:__struct__, _]},
          exception
        ]},
        {{:., _, [:erlang, :map_get]}, _, [:__exception__, _]}
      ]} ->
        [exception]

      _ ->
        []
    end
  end

end
