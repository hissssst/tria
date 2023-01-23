defmodule Tria.Compiler do

  @moduledoc """
  Interface for compiling Elixir mix projects.
  """

  @typedoc """
  Kind of definition
  """
  @type kind :: :def | :defp | :defmacro | :defmacrop

  @typedoc """
  Name of function
  """
  @type name :: atom()

  @typedoc """
  Universal signature of a function. That is a `module.kind_function/arity`
  """
  @type signature :: {module(), kind(), name(), arity()}

  @typedoc """
  One clause of a function in a `def` style
  """
  @type clause :: {args :: [Tria.t()], guards :: [Tria.t()], body :: Tria.t()}

  @typedoc """
  One definition. That is a `module.kind_function/arity do clauses`
  """
  @type definition :: {signature(), clauses :: [clause()]}

  @typedoc """
  `fn` ast in tria or elixir language
  """
  @type the_fn :: {:fn, list(), [Tria.t() | Macro.t()]}

  import Tria.Language
  alias Tria.Compiler.ContextServer
  alias Tria.Compiler.AbstractTranslator
  alias Tria.Compiler.ElixirTranslator
  alias Tria.Debug.Tracer
  alias Tria.Language.Codebase
  alias Tria.Language.Binary

  @special_functions [__info__: 1, __struct__: 2, __struct__: 1, __impl__: 1]

  defguard is_special(function, arity) when
    {function, arity} in @special_functions

  ## Project compilation pipeline

  @typedoc """
  Options for compiling Elixir project

  - `:context` -- (required) tria context module to compile given paths to
  - `:build_path` -- (required) the path which will be used to store the .beam files
  """
  @type compile_option :: {:context, module()}
  | {:build_path, Path.type(:absolute)}

  @doc """
  Entry point in a compilation process.

  This function performs these operations in such order

  1. File is compiled using `Kernel.ParallelCompiler.compile/2`

  2. module name and `.beam` bytecode sent back to the caller

  3. Caller calls `recompile_from_beam/3`
  """
  @spec compile([Path.type(:absolute)], [compile_option()] | map()) :: [{module(), binary()}]
  def compile(paths, opts) when is_list(opts), do: compile(paths, Map.new(opts))
  def compile(paths, %{build_path: build_path, context: context} = opts) do
    caller = self()

    with_compiler_options([ignore_module_conflict: true], fn ->
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
    end)
  end

  @spec compiled() :: list()
  defp compiled do
    # I feel very smart about this
    receive do
      {:compiled, message} -> [message | compiled()]
      after 0 -> []
    end
  end

  @spec with_compiler_options(Keyword.t(), (() -> any())) :: any()
  defp with_compiler_options(options, func) do
    was =
      Enum.map(options, fn {key, value} ->
        was = Code.get_compiler_option(key)
        Code.put_compiler_option(key, value)
        {key, was}
      end)

    try do
      func.()
    after
      Enum.each(was, fn {key, value} ->
        Code.put_compiler_option(key, value)
      end)
    end
  end

  @typedoc """
  Options which used upon recompilation existing beam file.
  It uses the same options as `compile/2` but with one extra option:

  - `:file` -- (required) filename of the module

  This function performs these operations:

  1. Extracts `abstract_code` from beam

  2. Extracts attributes, definitions, etc.

  3. Emits definitions to the context server

  4. Generates stub module which has corrects attributes
  but delegates each call to context module (just like `defdelegate` does)
  """
  @type recompile_option :: compile_option() | {:file, binary()}

  @spec recompile_from_beam(module(), binary(), [recompile_option()] | map()) :: {module(), binary()}
  def recompile_from_beam(module, binary, opts) when is_list(opts) do
    recompile_from_beam(module, binary, Map.new(opts))
  end
  def recompile_from_beam(module, binary, %{context: context, file: file}) do
    {:ok, ac} = Codebase.fetch_abstract_code(binary)
    {:ok, public} = Codebase.fetch_attribute(ac, :export)

    locals =
      for {:function, _anno, name, arity, _clauses} <- ac do
        {name, arity}
      end

    {specs, types, callbacks} = attributes_from_abstract(ac)
    docs = fetch_docs(binary)

    delegations =
      for {:function, anno, name, arity, clauses} <- ac do
        {kind, arity} = kind_of(name, arity, public)
        name = unmacro_name(name)

        signature = {module, kind, name, arity}
        fn_clauses =
          Tracer.with_local_trace(signature, fn ->
            clauses
            |> Tracer.tag(label: :abstract, pretty: true)
            |> AbstractTranslator.to_tria!(env: empty_env(module, file), locals: locals)
            |> remotify_local_calls(module, locals)
            |> put_file(file)
            |> Tracer.tag(label: :after_translation)
          end)

        clauses = fn_to_clauses(fn_clauses)
        definition = {signature, clauses}

        if not is_special(name, arity) do
          ContextServer.emit_definition(context, definition)
        end

        doc = Map.get(docs, {name, arity})
        spec = Map.get(specs, {name, arity})
        meta = AbstractTranslator.meta(anno)

        delegate(context, definition, meta, doc: doc, spec: spec)
      end

    ContextServer.mark_ready(context, module)
    body = {:__block__, [], delegations}
    body = prepend_attrs(body, callbacks ++ types)

    [{^module, binary}] =
      {:defmodule, [], [module, [do: body]]}
      # |> inspect_ast(label: :delegating)
      |> Code.compile_quoted(file)

    {module, binary}
  end

  defp fetch_docs(binary) do
    #FIXME 'Docs' is unstable. Improve fetching
    {:ok, doc} = Codebase.fetch_chunk(binary, 'Docs')
    {:docs_v1, _, :elixir, "text/markdown", _, _, docs} = :erlang.binary_to_term(doc)
    for {{k, name, arity}, _, _, %{"en" => doc}, _} when k in ~w[macro function]a <- docs, into: %{} do
      {{name, arity}, doc}
    end
  end

  # TODO separate specs for private functions from other attributes
  # and remove private functions from delegations to avoid a ton of warnings
  defp attributes_from_abstract(abstract_code) do
    Enum.reduce(abstract_code, {%{}, [], []}, fn
      {:attribute, _, s, {{:__info__, 1}, _}}, acc when s in ~w[spec callback]a ->
        acc

      {:attribute, _, :spec, {{name, arity}, [spec]}}, {specs, types, callbacks} ->
        quoted = Code.Typespec.spec_to_quoted(name, spec)
        {Map.put(specs, {name, arity}, quoted), types, callbacks}

      {:attribute, _, :callback, {{name, _arity}, [spec]}}, {specs, types, callbacks} ->
        quoted = Code.Typespec.spec_to_quoted(name, spec)
        {specs, types, [{:callback, quoted} | callbacks]}

      {:attribute, _, attrname, type}, {specs, types, callbacks} when attrname in ~w[type opaque typep]a ->
        quoted = Code.Typespec.type_to_quoted(type)
        {specs, [{attrname, quoted} | types], callbacks}

      _, acc ->
        acc
    end)
  end

  defp remotify_local_calls(ast, module, locals) do
    postwalk(ast, fn
      # This clauses fixes what the next clause does in binary patterns
      {:<<>>, _meta, _parts} = binary ->
        Binary.traverse_binary_specifiers(binary, fn
          dot_call(^module, name, args, meta, _meta) ->
            {name, meta, args}

          item ->
            item
        end)

      {name, meta, args} = ast_node when is_atom(name) and is_list(args) ->
        if {name, length(args)} in locals do
          dot_call(module, name, args, meta, meta)
        else
          ast_node
        end

      other ->
        other
    end)
  end

  ## This function create an implementation of the function for delegating module
  ## Generally, you want to make all regular functions call context implementation
  ## and all other function left as is. But some stuff like `__info__/1` requires
  ## decompilation and fetching struct fields

  # We do not define private functions in delegating module, since they will not ever be called
  defp delegate(_, {{_, private, _, _}, _}, _, _) when private in ~w[defp defmacrop]a, do: nil

  # We parse `__info__` to find if it defines structure
  defp delegate(_, {{_module, :def, :__info__, 1}, clauses}, _, _) do
    clauses
    |> Enum.find_value(fn
      {[:struct], _, [do: value]} -> value
      _ -> false
    end)
    |> case do
      nil -> nil
      fields ->
        required = for {:%{}, _, [field: field, required: true]} <- fields, do: field
        {:@, [], [{:enforce_keys, [], [required]}]}
    end
  end

  # Leave struct as is, because it is faster to call it that way
  defp delegate(_, {{module, :def, :__struct__, 0}, [{[], _, [do: body]}]}, _, _) do
    Kernel.Utils.announce_struct(module)
    case body do
      {:%{}, _, pairs} ->
        pairs
        |> Keyword.delete(:__struct__)
        |> ElixirTranslator.from_tria()
        |> Keyword.pop(:__exception__)
        |> case do
          {nil, pairs} -> {:defstruct, [], [pairs]}
          {_, pairs} -> {:defexception, [], [pairs]}
        end

      _ ->
        nil
    end
  end

  # Just leave the __struct__/1 alone
  defp delegate(_, {{_module, :def, :__struct__, 1}, _}, _, _) do
    nil
  end

  # Leave impl as is, because it will be faster to dispatch
  defp delegate(_, {{_, :def, :__impl__, 1}, _} = definition, _, attrs) do
    definition
    |> define_definition()
    |> prepend_attrs(attrs)
  end

  # Leave delegate defmacros, but we also prepend __CALLER__
  defp delegate(tria_context, {{module, :defmacro, name, arity} = signature, _clauses}, meta, attrs) do
    args = Macro.generate_unique_arguments(arity, nil)
    callargs = [{:__CALLER__, [], nil} | args]
    fname = fname(signature)

    {:defmacro, meta, [{name, meta, args}, [do: dot_call(tria_context, fname, callargs, meta, meta)]]}
    |> prepend_attrs(attrs)
    |> Tracer.tag_ast(key: {module, name, arity}, label: :delegated)
  end

  # Delegate regular functions
  defp delegate(tria_context, {{module, kind, name, arity} = signature, _}, meta, attrs) do
    args = Macro.generate_unique_arguments(arity, nil)
    fname = fname(signature)

    {kind, meta, [{name, meta, args}, [do: dot_call(tria_context, fname, args, meta, meta)]]}
    |> prepend_attrs(attrs)
    |> Tracer.tag_ast(key: {module, name, arity}, label: :delegated)
  end

  defp prepend_attrs(quoted, []), do: quoted
  defp prepend_attrs(quoted, [{_name, nil} | tail]) do
    prepend_attrs(quoted, tail)
  end
  defp prepend_attrs({:__block__, _, lines}, [{name, value} | tail]) do
    quote do
      @(unquote(name)(unquote(value)))
      unquote_splicing lines
    end
    |> prepend_attrs(tail)
  end
  defp prepend_attrs(quoted, [{name, value} | tail]) do
    quote do
      @(unquote(name)(unquote(value)))
      unquote quoted
    end
    |> prepend_attrs(tail)
  end

  defp put_file(ast, file) do
    prewalk(ast, fn
      {left, meta, right} ->
        {left, Keyword.put_new(meta, :file, file), right}

      other ->
        other
    end)
  end

  ## Other public functions

  def save(build_path, module, binary) do
    filename = Path.join(build_path, "#{module}.beam")
    File.write!(filename, binary)

    :code.add_path to_charlist Path.dirname filename
    :code.purge module
    :code.load_file module
  end

  @doc """
  Like `Code.compile_quoted/2` but notes the place where
  compilation error occured
  """
  @spec compile_quoted(Macro.t(), Path.t()) :: [{module(), binary()}]
  def compile_quoted(quoted, file \\ "nofile") do
    Code.compile_quoted(quoted, file)
  rescue
    CompileError ->
      stacktrace = __STACKTRACE__

      lined =
        postwalk(quoted, fn ast ->
          Macro.update_meta(ast, &Keyword.put(&1, :line, :erlang.unique_integer([:positive])))
        end)

      try do
        Code.compile_quoted(lined, file)
      rescue
        e in CompileError ->
          inspect_ast(lined, label: :failed_to_compile, with_contexts: true, highlight_line: e.line)
          reraise e, stacktrace
      end
  end

  @doc """
  Converts module, name to the name in the context module
  """
  @spec fname(signature()) :: atom()
  def fname({module, kind, name, _arity}) do
    kind_name =
      case kind do
        :defmacro -> "macro_#{name}"
        _ -> "#{name}"
      end

    module
    |> Module.split()
    |> Enum.map(&snake/1)
    |> Kernel.++([kind_name])
    |> Enum.join("_")
    |> String.to_atom()
  end

  @doc """
  Converts list of clauses to fn
  """
  @spec clauses_to_fn([clause()]) :: the_fn()
  def clauses_to_fn(clauses) when is_list(clauses) do
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
  def clauses_to_fn(args, guards, body) do
    clauses_to_fn [{args, guards, body}]
  end

  @doc """
  Converts fn to `def`'s `{args, guards, body}` clause or clauses
  """
  @spec fn_to_clauses(the_fn()) :: [clause()]
  def fn_to_clauses({:fn, _, fn_clauses}) do
    fn_to_clauses(fn_clauses)
  end

  def fn_to_clauses(fn_clauses) when is_list(fn_clauses) do
    Enum.map(fn_clauses, fn
      {:"->", _, [args, {:try, _, [body]}]} ->
        {guards, args} = pop_guards(args)
        {args, guards, body}

      {:"->", _, [args, body]} ->
        {guards, args} = pop_guards(args)
        {args, guards, [do: body]}
    end)
  end

  # Sorry for this silly name
  def define_definition({{_module, kind, name, _arity}, clauses}) do
    import ElixirTranslator, only: [from_tria: 1]

    defs =
      clauses
      |> Enum.map(fn {args, guards, body} -> {from_tria(args), from_tria(guards), from_tria(body)} end)
      |> Enum.map(fn
        {args, [], body} ->
          quote do: unquote(kind)(unquote(name)(unquote_splicing args), unquote(body))

        {args, guards, body} ->
          quote do: unquote(kind)(unquote(name)(unquote_splicing args) when unquote(join_guards guards), unquote(body))
      end)

    {:__block__, [], defs}
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


end
