defmodule Tria.Compiler.ContextServer do

  @moduledoc """
  GenServer which compiles the context module when all dependants are compiled

  #TODO implement recompilation of existing module
  """

  use GenServer
  import Tria.Common

  alias Tria.Translator.Elixir, as: ElixirTranslator
  alias Tria.Compiler

  # Public

  def emit_definition(context, definition) do
    GenServer.call(context, {:add_definition, definition}, :infinity)
  end

  def mark_ready(context, module) do
    GenServer.call(context, {:ready, module}, :infinity)
  end

  def generate(context) do
    GenServer.call(context, :generate, :infinity)
  end

  def evaluate(context, mfarity, args) do
    GenServer.call(context, {:evaluate, mfarity, args}, :infinity)
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
    generate_stub(state)
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
    quoted = quote do: unquote(Compiler.def_to_fn(clauses)).(unquote_splicing args)
    Tria.Interpreter.eval!(quoted, [], :infinity)
  end

  defp generate_stub(%{name: context, definitions: definitions} = state) do
    funcs =
      Enum.map(definitions, fn {{module, name, arity}, {kind, _clauses}} ->
        fname = Compiler.fname(module, name)

        args = for _ <- List.duplicate(nil, arity), do: {:arg, [counter: gen_uniq_context()], Elixir}
        args =
          case kind do
            :defmacro -> [{:caller, [], :tria_caller} | args]
            _ -> args
          end

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

    quote do
      defmodule unquote state.name do
        unquote_splicing funcs
      end
    end
    |> inspect_ast(label: :gen)
    |> Compiler.compile_quoted("#{state.name}.ex")
  end

  defp definitions_to_funcs(definitions) do
    Enum.flat_map(definitions, fn {{module, name, _arity}, {kind, clauses}} ->
      fname = Compiler.fname(module, name)
      clauses = run(clauses, definitions)
      def_to_func(kind, fname, clauses)
    end)
  end

  defp def_to_func(kind, fname, clauses) when kind in ~w[defmacro defmacrop]a do
    clauses = add_caller(clauses)
    def_to_func(:def, fname, clauses)
  end

  defp def_to_func(kind, fname, clauses) do
    Enum.map(clauses, fn {args, guards, body} ->
      [args, guards, body] = ElixirTranslator.from_tria [args, guards, body]
      define(kind, fname, args, guards, body)
    end)
  end

  # Transforms the `{args, guards, body}` to the `fn`, runs optimizers
  # and transforms the result back to `{args, guards, body}`
  defp run(clauses, definitions) do
    clauses
    |> Compiler.def_to_fn()
    |> Tria.run(translate: false)
    |> contextify_local_calls(definitions)
    |> Compiler.fn_to_def()
  end

  defp contextify_local_calls(ast, definitions) do
    postwalk(ast, fn
      dot_call(module, function, args, _, callmeta) = call when is_atom(module) and is_atom(function) ->
        mfarity = {module, function, length(args)}
        case definitions do
          %{^mfarity => _} -> {Compiler.fname(module, function), callmeta, args}
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
    guard = guards_to_guard(guards)
    quote do
      unquote(kind)(unquote(fname)(unquote_splicing args) when unquote(guard), unquote(body))
    end
  end

  defp guards_to_guard(guards) do
    Enum.reduce(guards, fn right, left -> {:when, [], [left, right]} end)
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
