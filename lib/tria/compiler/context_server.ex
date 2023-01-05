defmodule Tria.Compiler.ContextServer do

  @moduledoc """
  GenServer which compiles the context module when all dependants are compiled

  #TODO implement recompilation of existing module
  """

  use GenServer
  import Tria.Language

  alias Tria.Compiler
  alias Tria.Compiler.ElixirTranslator
  alias Tria.Debug.Tracer
  alias Tria.Language.FunctionRepo
  alias Tria.Language.Interpreter
  alias Tria.Optimizer

  # Public

  defp call(context, msg) do
    context
    |> start()
    |> GenServer.call(msg, :infinity)
  end

  def emit_definition(context, definition) do
    call(context, {:add_definition, definition})
  end

  def mark_ready(context, module) do
    call(context, {:ready, module})
  end

  def generate(context) do
    call(context, :generate)
  end

  def evaluate(context, mfarity, args) do
    call(context, {:evaluate, mfarity, args})
  end

  def start(name) do
    case GenServer.start(__MODULE__, %{name: name}, name: name) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  # GenServer

  def init(opts) do
    state = %{
      definitions: %{},
      name: Map.fetch!(opts, :name)
    }

    {:ok, state}
  end

  def handle_call({:evaluate, mfarity, args}, from, %{definitions: definitions} = state) do
    {kind, clauses} = Map.fetch!(definitions, mfarity)
    spawn fn ->
      result = do_evaluate(kind, clauses, args)
      GenServer.reply(from, result)
    end
    {:noreply, state}
  end

  def handle_call({:add_definition, {{module, kind, name, arity}, clauses}}, _from, %{definitions: definition} = state) do
    definition = Map.put(definition, {module, name, arity}, {kind, clauses})
    {:reply, :ok, %{state | definitions: definition}}
  end

  def handle_call({:ready, _module}, _from, state) do
    # We generate stub every time the module is ready,
    # because __struct__ behaves this way
    # generate_stub(state)
    {:reply, :ok, state}
  end

  def handle_call(:generate, _from, state) do
    modules = generate_context(state)
    {:reply, modules, state}
  end

  # Helpers

  defp do_evaluate(macrokind, clauses, args) when macrokind in ~w[defmacro defmacrop]a do
    clauses = add_caller(clauses)
    args = for arg <- args, do: Macro.escape arg
    do_evaluate(:def, clauses, args)
  end

  defp do_evaluate(_kind, clauses, args) do
    quoted = quote do: unquote(Compiler.clauses_to_fn(clauses)).(unquote_splicing args)
    Interpreter.eval!(quoted, [], :infinity)
  end

  defp generate_stub(%{name: context, definitions: definitions} = state) do
    funcs =
      Enum.map(definitions, fn {{module, name, arity}, {kind, _clauses}} ->
        fname = Compiler.fname({module, kind, name, arity})
        args = for _ <- List.duplicate(nil, arity), do: {:arg, [counter: gen_uniq_context()], Elixir}

        body = [do: dot_call(__MODULE__, :evaluate, [context, Macro.escape({module, name, arity}), args])]
        define(:def, fname, args, [], body)
      end)

    quote do
      defmodule unquote state.name do
        unquote_splicing funcs
      end
    end
    # |> inspect_ast(label: :stub)
    |> Compiler.compile_quoted("#{state.name}.ex")
  end

  defp generate_context(state) do
    funcs = definitions_to_funcs(state.definitions)

    #FIXME add functions to module, instead of recompiling it every time
    unless :code.get_object_code(state.name) == :error do
      IO.warn "The context module already exists"
    end

    module =
      quote do
        defmodule unquote state.name do
          unquote_splicing funcs
        end
      end

    File.write!("tria_global_context.ex", ast_to_string(module))

    Compiler.compile_quoted(module, "#{state.name}.ex")
  end

  defp definitions_to_funcs(definitions) do
    definitions
    |> Enum.map(fn {{module, name, arity}, {kind, clauses}} ->
      the_fn = Compiler.clauses_to_fn(clauses)
      FunctionRepo.insert({module, name, arity}, :tria, the_fn)
      {{module, kind, name, arity}, the_fn}
    end)
    |> Enum.flat_map(fn {{module, kind, name, arity} = signature, the_fn} ->
      fname = Compiler.fname(signature)

      clauses =
        try do
          Tracer.with_local_trace({module, name, arity}, fn ->
            the_fn
            |> Tracer.tag_ast(label: :before_passes)
            |> Optimizer.run(remove_unused: false)
            |> contextify_local_calls(definitions)
            |> Tracer.tag_ast(label: :generating)
            |> Compiler.fn_to_clauses()
          end)
        rescue
          e ->
            IO.puts "Failed generation for #{module}.#{name}/#{arity}"
            reraise e, __STACKTRACE__
        end

      create_definition(kind, fname, clauses)
    end)
  end

  defp create_definition(kind, fname, clauses) when kind in ~w[defmacro defmacrop]a do
    create_definition(:def, fname, clauses)
  end

  defp create_definition(kind, fname, clauses) do
    Enum.map(clauses, fn {args, guards, body} ->
      [args, guards, body] = ElixirTranslator.from_tria [args, guards, body]
      define(kind, fname, args, guards, body)
    end)
  end

  defp contextify_local_calls(ast, definitions) do
    postwalk(ast, fn
      dot_call(module, name, args, _, callmeta) = call when is_atom(module) and is_atom(name) ->
        arity = length(args)
        mfarity = {module, name, arity}
        case definitions do
          %{^mfarity => {kind, _clauses}} -> {Compiler.fname({module, kind, name, arity}), callmeta, args}
          _ -> call
        end

      other ->
        other
    end)
  end

  defp define(kind, fname, args, [], body) do
    quote do
      unquote(kind)(unquote(fname)(unquote_splicing args), unquote body)
    end
  end

  defp define(kind, fname, args, guards, body) do
    guard = Compiler.join_guards(guards)
    quote do
      unquote(kind)(unquote(fname)(unquote_splicing args) when unquote(guard), unquote(body))
    end
  end

  defp add_caller(clauses, caller_var \\ {:caller, [], :tria_caller}) do
    Enum.map(clauses, fn {args, guards, body} ->
      args = [caller_var | args]
      body = replace_caller(body, caller_var)

      {args, guards, body}
    end)
  end

  defp replace_caller(ast, replacement) do
    postwalk(ast, fn
      caller when is_CALLER(caller) -> replacement
      other -> other
    end)
  end

end
