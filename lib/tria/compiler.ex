defmodule Tria.Compiler do

  @moduledoc """
  Entry-point for compiling Elixir projects.
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
  import Tria.Language.Guard
  alias Tria.Compiler.AbstractTranslator
  alias Tria.Compiler.Annotations
  alias Tria.Compiler.ContextServer
  alias Tria.Compiler.ElixirCompiler
  alias Tria.Compiler.ElixirTranslator
  alias Tria.Compiler.Manifest
  alias Tria.Debug
  alias Tria.Debug.Tracer
  alias Tria.Language.Binary
  alias Tria.Language.Beam
  alias Tria.Language.FunctionRepo
  alias Tria.Language.FunctionGraph

  @special_functions [__info__: 1, __struct__: 2, __struct__: 1, __impl__: 1]

  defguard is_special(function, arity) when
    {function, arity} in @special_functions

  ## Project compilation pipeline

  @typedoc """
  Options for compiling Elixir project

  - `:context` -- (required) tria context module to compile given paths to
  - `:build_path` -- (required) the path which will be used to store the .beam files
  - `:manifest` -- manifest of already compiled project. When omitted, project is recompiled from scratch
  """
  @type compile_option :: {:context, module()}
  | {:build_path, Path.type(:absolute)}
  | {:manifest, Manifest.t()}

  @doc """
  Entry point in a compilation process.

  This function performs these operations in such order

  1. File is compiled using `Kernel.ParallelCompiler.compile/2`

  2. module name and `.beam` bytecode sent back to the caller

  3. Caller calls `recompile_from_beam/3`
  """
  @spec compile([Path.type(:absolute)], [compile_option()] | map()) :: {Manifest.t(), [{module(), binary()}]}
  def compile(paths, opts) when is_list(opts), do: compile(paths, Map.new(opts))
  def compile(paths, opts) when is_map(opts) do
    %{manifest: manifest} = opts = Map.put_new_lazy(opts, :manifest, fn -> %Manifest{} end)

    changed_files =
      manifest
      |> Manifest.diff_files(paths)
      |> Debug.inspect(label: :changed_files)

    compile_files_diff(changed_files, paths, opts)
  end

  defp compile_files_diff([], _, %{manifest: manifest}), do: {manifest, []}
  defp compile_files_diff(files_diff, _paths, %{build_path: build_path, context: context, manifest: manifest} = opts) do
    caller = self()
    added_and_changed = for {x, file} when x in ~w[added changed]a <- files_diff, do: file

    ElixirCompiler.parallel_compile(added_and_changed, [
      # Code options
      ignore_module_conflict: true,
      no_warn_undefined: :all,
      trace_deps: true,
      debug_info: true,

      # ParallelCompiler options
      dest: build_path,
      each_module: fn file, module, bytecode ->
        Debug.inspect(module, label: :each_module)
        Beam.store_object_code(module, bytecode)
        send(caller, {:last_cycle_compiled, {module, bytecode, file}})
      end,
      each_cycle: fn graphs ->
        modules =
          for {module, bytecode, file} <- compiled(:last_cycle_compiled) do
            send(caller, {:compiled, {module, bytecode, file}})
            module
          end

        for {graph, links} <- graphs, do: FunctionGraph.relink_many(graph, links)

        manifest = Manifest.reflect_graphs(manifest)
        dependant_modules = Manifest.compile_time_dependants(manifest, modules)
        files = Manifest.infer_files(manifest, dependant_modules)

        Debug.inspect(modules, label: :modules)
        Debug.inspect(dependant_modules, label: :dependants)
        Debug.inspect(files, label: :files_of_dependants)

        {:compile, files, []}
      end
    ])

    changed_files = for {:changed, file} <- files_diff, do: file

    compiled = compiled(:compiled)
    compiled_modules = MapSet.new(compiled, fn {module, _, _} -> module end)
    was_modules = Manifest.infer_modules(manifest, changed_files)

    new_modules = MapSet.difference(compiled_modules, was_modules)
    removed_modules = MapSet.difference(was_modules, compiled_modules)
    changed_modules = MapSet.intersection(compiled_modules, was_modules)

    Debug.inspect(new_modules, label: :new_modules)
    Debug.inspect(removed_modules, label: :removed_modules)
    Debug.inspect(changed_modules, label: :changed_modules)

    file_to_modules =
      Enum.reduce(compiled, %{}, fn {module, _, file}, acc ->
        Map.update(acc, file, MapSet.new([module]), &MapSet.put(&1, module))
      end)

    manifest =
      manifest
      |> Manifest.apply_diff(files_diff)
      |> Manifest.update_file_to_modules(file_to_modules)

    ContextServer.restore(opts.context)
    ContextServer.emit_difference(opts.context, new_modules, removed_modules, changed_modules)

    modules =
      Enum.map(compiled, fn {module, bytecode, file} ->
        recompile_from_beam(module, bytecode, Map.put(opts, :file, file))
      end)

    context_modules = ContextServer.generate(context)
    modules = context_modules ++ modules

    manifest = Manifest.reflect_graphs(manifest)
    {manifest, modules}
  end

  @spec compiled(any()) :: list()
  defp compiled(prefix) do
    receive do
      {^prefix, message} -> [message | compiled(prefix)]
      after 0 -> []
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
    docs = fetch_docs(module, binary)
    ac = Beam.abstract_code!(binary)

    %{
      export: public,
      tria_acc: annotations,
      callback: callbacks,
      type: type,
      typep: typep,
      opaque: opaque,
      spec: specs
    } = Beam.attributes(ac, ~w[callback export tria_acc type typep opaque spec]a)

    for {{name, arity}, _} <- callbacks do
      FunctionRepo.insert({module, name, arity}, :callback, true)
    end

    public = Enum.concat(public)

    types = prepare_types(type, typep, opaque)
    specs = prepare_specs(specs)
    callbacks = prepare_callbacks(callbacks)

    locals = for {:function, _anno, name, arity, _clauses} <- ac, do: {name, arity}

    annotations
    |> Enum.map(fn [{kind, name, arity, opts}] -> {{module, kind, name, arity}, opts} end)
    |> Annotations.put_annotations()

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
            |> Tracer.tag(label: :after_translation, pretty: true)
          end)

        FunctionRepo.insert({module, name, arity}, :defined, true)

        clauses = fn_to_clauses(fn_clauses)
        definition = {signature, clauses}

        if not is_special(name, arity) do
          ContextServer.emit_definition(context, definition)
        end

        doc = Map.get(docs, {name, arity})
        specs = for spec <- Map.get(specs, {name, arity}, []), do: {:spec, spec}
        meta = AbstractTranslator.meta(anno)

        delegate(context, definition, meta, [{:doc, doc} | specs])
      end

    ContextServer.mark_ready(context, module)

    body =
      {:__block__, [], delegations}
      |> prepend_attrs(callbacks ++ types)
      |> put_file(file)

    [{^module, binary}] =
      {:defmodule, [file: file], [module, [do: body]]}
      |> ElixirCompiler.compile_quoted(file: file, ignore_module_conflict: true, no_warn_undefined: :all)

    {module, binary}
  end

  defp fetch_docs(module, binary) do
    try do
      {:ok, doc} = Beam.chunk(binary, ~c"Docs")
      :erlang.binary_to_term(doc)
    rescue
      ArgumentError ->
        Debug.puts "Non BERT docs format for #{module}, continuing without docs"
        %{}

      MatchError ->
        Debug.puts "Failed to fetch docs chunk for #{module}, continuing without docs"
        %{}
    else
      {:docs_v1, _, :elixir, "text/markdown", _, _, docs} ->
        for {{k, name, arity}, _, _, %{"en" => doc}, _} when k in ~w[macro function]a <- docs, into: %{} do
          {{name, arity}, doc}
        end

      docs ->
        Debug.puts "Unexpected docs format for #{module}, #{inspect docs}, continuing without docs"
        %{}
    end
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

  ### Helpers for preparing module attributes

  defp prepare_types(type, typep, opaque) do
    [
      Enum.map(type, fn x -> {:type, x} end),
      Enum.map(typep, fn x -> {:typep, x} end),
      Enum.map(opaque, fn x -> {:opaque, x} end)
    ]
    |> Enum.concat()
    |> Enum.map(fn {kind, type} ->
      {kind, Code.Typespec.type_to_quoted(type)}
    end)
  end

  defp prepare_specs(specs) do
    Enum.reduce(specs, %{}, fn
      {{:__info__, 1}, _}, acc ->
        acc

      {{name, _arity} = key, values}, acc ->
        values = Enum.map(values, &Code.Typespec.spec_to_quoted(name, &1))
        Map.put(acc, key, values)
    end)
  end

  defp prepare_callbacks(callbacks) do
    Enum.map(callbacks, fn {{name, _arity}, [value]} ->
      {:callback, Code.Typespec.spec_to_quoted(name, value)}
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

  @spec prepend_attrs(Macro.t(), [{atom(), nil | Macro.t()}]) :: Macro.t()
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

  @doc """
  Converts module and name to the name in the context module
  """
  @spec fname(signature()) :: atom()
  def fname({module, kind, name, _arity}) do
    kind_name =
      case kind do
        :defmacro -> "_macro_#{name}"
        _ -> "_#{name}"
      end

    module
    |> Module.split()
    |> Kernel.++([kind_name])
    |> Enum.join("_")
    |> String.to_atom()
  end

  @spec unfname(atom()) :: {module(), atom()}
  def unfname(name) do
    [module | function] =
      name
      |> Atom.to_string()
      |> String.split("__")

    function = Enum.join(function, "__")

    module =
      module
      |> String.split("_")
      |> Module.concat()

    {module, String.to_atom(function)}
  end

  @doc """
  Converts list of clauses to fn
  """
  @spec clauses_to_fn([clause()]) :: the_fn()
  def clauses_to_fn(clauses) when is_list(clauses) do
    fn_clauses =
      Enum.flat_map(clauses, fn {args, guards, body} ->
        args = append_guards(args, guards)
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
          quote do: unquote(kind)(unquote(name)(unquote_splicing args) when unquote(join_when guards), unquote(body))
      end)

    {:__block__, [], defs}
  end

  # Helpers

  defp kind_of(name, arity, public) do
    name
    |> to_string()
    |> String.starts_with?("MACRO-")
    |> if do
      if {name, arity} in public do
        {:defmacro, arity - 1}
      else
        {:defmacrop, arity - 1}
      end
    else
      if {name, arity} in public do
        {:def, arity}
      else
        {:defp, arity}
      end
    end
  end

  defp unmacro_name(name) do
    case to_string name do
      "MACRO-" <> name -> String.to_atom(name)
      _ -> name
    end
  end


end
