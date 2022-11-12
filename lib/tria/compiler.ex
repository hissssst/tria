defmodule Tria.Compiler do

  defp env(module, file) do
    %Macro.Env{__ENV__ |
      context: nil,
      versioned_vars: %{},
      module: module,
      file: file
    }
  end

  @typedoc """
  Kind of definition
  """
  @type kind :: :def | :defp | :defmacro | :defmacrop

  @typedoc """
  Name of function
  """
  @type name :: atom()

  @typedoc """
  Universal signature of a function
  """
  @type signature :: {module(), kind(), name(), arity()}

  @typedoc """
  One clause of a function in a `def` style
  """
  @type clause :: {args :: [Tria.t()], guards :: [Tria.t()], body :: Tria.t()}

  @typedoc """
  One definition
  """
  @type definition :: {signature(), clauses :: [clause()]}

  @moduledoc """
  Helpers for compiling Elixir modules
  """

  import Tria.Common
  alias Tria.Compiler.ContextServer
  alias Tria.Tracer

  # Project compilation pipeline

  def compile(paths, opts) when is_list(opts), do: compile(paths, Map.new(opts))
  def compile(paths, %{build_path: build_path, context: context} = opts) do
    caller = self()

    Kernel.ParallelCompiler.compile(paths, [
      dest: build_path,
      each_module: fn file, module, bytecode ->
        send(caller, {:compiled, {module, bytecode, file}})
      end
    ])

    modules =
      Enum.map(compiled(), fn {module, bytecode, file} ->
        opts = Map.put(opts, :file, file)
        Code.ensure_compiled!(module)
        recompile_from_beam(module, bytecode, opts)
      end)

    context_modules = ContextServer.generate(context)
    context_modules ++ modules
  end

  defp compiled do
    receive do
      {:compiled, message} -> [message | compiled()]
      after 0 -> []
    end
  end

  def recompile_from_beam(module, binary, %{context: context, file: file}) do
    alias Tria.Codebase
    alias Tria.Translator.Abstract

    {:ok, ac} = Codebase.fetch_abstract_code(binary)
    {:ok, public} = Codebase.fetch_attribute(ac, :export)

    locals =
      for {:function, _anno, name, arity, _clauses} when name != :__info__ <- ac do
        {name, arity}
      end

    attributes = attributes_from_abstract(ac)
    docs = fetch_docs(binary)

    delegations =
      for {:function, _anno, name, arity, clauses} when name != :__info__ <- ac do
        {kind, arity} = kind_of(name, arity, public)
        name = unmacro_name(name)

        signature = {module, kind, name, arity}
        fn_clauses =
          clauses
          |> Tracer.tag({module, name, arity}, label: :abstract)
          |> Abstract.to_tria!(env(module, file))
          |> remotify_local_calls(module, locals)

        clauses = fn_to_def(fn_clauses)
        definition = {signature, clauses}
        ContextServer.emit_definition(context, definition)

        doc = Map.get(docs, {name, arity})
        delegate(context, signature, doc)
      end

    ContextServer.mark_ready(context, module)
    body = {:__block__, [], attributes ++ delegations}
    |> inspect_ast(label: module)

    [{_, binary}] = Code.compile_quoted({:defmodule, [], [module, [do: body]]}, file)
    {module, binary}
  end

  defp fetch_docs(binary) do
    {:ok, doc} = Tria.Codebase.fetch_chunk(binary, 'Docs')
    {:docs_v1, _, :elixir, "text/markdown", _, _, docs} = :erlang.binary_to_term(doc)
    for {{k, name, arity}, _, _, %{"en" => doc}, _} when k in ~w[macro function]a <- docs, into: %{} do
      {{name, arity}, doc}
    end
  end

  defp attributes_from_abstract([{:attribute, _, _, {{:__info__, _}, _}} | tail]) do
    attributes_from_abstract tail
  end
  defp attributes_from_abstract(
    [{:attribute, _anno, spec_or_callback, {{name, _}, [spec]}} | tail]
  ) when spec_or_callback in ~w[spec callback]a do
    quoted = Code.Typespec.spec_to_quoted(name, spec)
    attribute = {:"@", [import: Kernel], [{spec_or_callback, [], [quoted]}]}
    [attribute | attributes_from_abstract(tail)]
  end
  defp attributes_from_abstract([{:attribute, _anno, :type, value} | tail]) do
    quoted = Code.Typespec.type_to_quoted(value)
    attribute = {:"@", [import: Kernel], [{:type, [], [quoted]}]}
    [attribute | attributes_from_abstract(tail)]
  end
  defp attributes_from_abstract([_ | tail]) do
    attributes_from_abstract(tail)
  end
  defp attributes_from_abstract([]), do: []

  defp remotify_local_calls(ast, module, locals) do
    postwalk(ast, fn
      {name, _meta, args} = ast_node when is_atom(name) and is_list(args) ->
        if {name, length(args)} in locals do
          dot_call(module, name, args)
        else
          ast_node
        end

      other ->
        other
    end)
  end

  def save(build_path, module, binary) do
    filename = Path.join(build_path, "#{module}.beam")
    File.write!(filename, binary)

    :code.add_path to_charlist Path.dirname filename
    :code.purge module
    :code.load_file module
  end

  @doc """
  Like `Code.compile_quoted/2` but with error descriptions
  """
  def compile_quoted(quoted, file \\ "nofile") do
    try do
      Code.compile_quoted(quoted, file)
    rescue
      _ ->
        lined =
          postwalk(quoted, fn ast ->
            Macro.update_meta(ast, &Keyword.put(&1, :line, :erlang.unique_integer([:positive])))
          end)

        try do
          Code.compile_quoted(lined, file)
        rescue
          e in CompileError ->
            inspect_ast(lined, label: :failed_to_compile, with_contexts: true, highlight_line: e.line)
            reraise e, __STACKTRACE__
        end
    end
  end

  @doc """
  Converts module, name to the name in the context module
  """
  def fname(module, name) do
    module
    |> Module.split()
    |> Enum.map(&snake/1)
    |> Kernel.++([to_string name])
    |> Enum.join("_")
    |> String.to_atom()
  end

  @doc """
  Converts `def`'s `{args, guards, body}` clause or clauses to fn
  """
  def def_to_fn(clauses) when is_list(clauses) do
    fn_clauses =
      Enum.flat_map(clauses, fn {args, guards, body} ->
        args = add_guards(args, guards)
        body =
          case body do
            [do: body] -> body
            body -> {:try, [], [body]}
          end

        quote do
          unquote_splicing args -> unquote body
        end
      end)

    {:fn, [], fn_clauses}
  end
  def def_to_fn(args, guards, body) do
    def_to_fn [{args, guards, body}]
  end

  @doc """
  Converts fn to `def`'s `{args, guards, body}` clause or clauses
  """
  def fn_to_def({:fn, _, fn_clauses}) do
    fn_to_def(fn_clauses)
  end

  def fn_to_def(fn_clauses) when is_list(fn_clauses) do
    Enum.map(fn_clauses, fn
      {:"->", _, [args, {:try, _, [body]}]} ->
        {guards, args} = pop_guards(args)
        {args, guards, body}

      {:"->", _, [args, body]} ->
        {guards, args} = pop_guards(args)
        {args, guards, [do: body]}
    end)
  end

  # Helpers

  # Converts camel case strings to snake case
  defp snake(""), do: ""
  defp snake(<<first, second, rest :: binary>>) when first not in ?A..?Z and second in ?A..?Z do
    <<first, ?_, second - ?A + ?a>> <> snake(rest)
  end
  defp snake(<<upper, rest :: binary>>) when upper in ?A..?Z do
    <<upper - ?A + ?a>> <> snake(rest)
  end
  defp snake(<<other, rest :: binary>>) do
    <<other>> <> snake(rest)
  end

  defp add_guards(args, []), do: args
  defp add_guards(args, guards) do
    guard = join_guards guards
    [{:when, [], args ++ [guard]}]
  end

  def join_guards([guard]), do: guard
  def join_guards([guard | guards]) do
    {:when, [], [guard, join_guards(guards)]}
  end

  defp unjoin_guards({:when, _, [guard, guards]}), do: [guard | unjoin_guards guards]
  defp unjoin_guards(guard), do: [guard]

  defp pop_guards([{:when, _, args_and_guards}]) do
    {guards, args} = List.pop_at(args_and_guards, -1)
    {unjoin_guards(guards), args}
  end
  defp pop_guards(args), do: {[], args}

  defp kind_of(name, arity, public) do
    if String.starts_with?("#{name}", "MACRO-") do
      if {name, arity} in public,
        do: {:defmacro, arity - 1},
        else: {:defmacrop, arity - 1}
    else
      if {name, arity} in public, do: {:def, arity}, else: {:defp, arity}
    end
  end

  defp unmacro_name(name) do
    case to_string name do
      "MACRO-" <> name -> String.to_atom(name)
      _ -> name
    end
  end

  defp delegate(tria_context, {module, kind, name, arity}, doc) when kind in ~w[defmacro defmacrop]a do
    args = Macro.generate_unique_arguments(arity, nil)
    callargs = [{:__CALLER__, [], nil} | args]
    fname = fname(module, name)


    quote do
      unquote(kind)(unquote(name)(unquote_splicing args), do: unquote(tria_context).unquote(fname)(unquote_splicing callargs))
    end
    |> maybe_add_doc(doc)
    |> Tracer.tag_ast({module, name, arity}, label: :delegated)
  end
  defp delegate(tria_context, {module, kind, name, arity}, doc) do
    args = Macro.generate_unique_arguments(arity, nil)
    fname = fname(module, name)

    quote do
      unquote(kind)(unquote(name)(unquote_splicing args), do: unquote(tria_context).unquote(fname)(unquote_splicing args))
    end
    |> maybe_add_doc(doc)
    |> Tracer.tag_ast({module, name, arity}, label: :delegated)
  end

  defp maybe_add_doc(quoted, nil), do: quoted
  defp maybe_add_doc(quoted, doc) do
    quote do
      unquote doc && quote(do: @doc unquote(doc))
      unquote quoted
    end
  end

end
