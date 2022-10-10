defmodule Tria.Compiler.ContextServer do

  @moduledoc """
  GenServer which compiles the context module when all dependants are compiled

  #TODO implement recompilation of existing module
  """

  alias Tria.Translator.Elixir, as: ElixirTranslator
  use GenServer
  import Tria.Common, only: [inspect_ast: 2]

  # Public

  def emit_definition(context, kind, module, name, clauses) do
    GenServer.call(context, {:new_def, kind, module, name, clauses}, :infinity)
  end

  def mark_ready(context, module) do
    GenServer.call(context, {:ready, module}, :infinity)
    Process.sleep(1000)
  end

  def get_pdict() do
    Process.get() |> Keyword.put(:elixir_compiler_modules, [])
  end

  def start(name, pdict \\ get_pdict()) do
    case GenServer.start(__MODULE__, %{name: name, pdict: pdict}, name: name) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  def fname(module, name), do: :"#{module}_#{name}"
  # def fname(_module, name), do: name

  # GenServer

  def init(%{name: name, pdict: pdict}) do
    for {k, v} <- pdict, do: Process.put(k, v)
    {:ok, %{definitions: %{}, modules: MapSet.new(), name: name, tick: nil}}
  end

  def handle_call({:new_def, kind, module, name, clauses}, _from, %{definitions: defs, modules: mods} = state) do
    defs = Map.put(defs, {module, name}, {kind, clauses})
    {:reply, :ok, %{state | definitions: defs, modules: MapSet.put(mods, module)}}
  end

  def handle_call({:ready, module}, _from, %{modules: modules} = state) do
    modules = MapSet.delete(modules, module)
    state = tick(state)
    {:reply, :ok, %{state | modules: modules}}
  end

  def handle_info(:tick, %{modules: modules} = state) do
    if MapSet.new() == modules, do: create_module(state)
    {:noreply, state}
  end

  # Helpers

  defp tick(%{tick: nil} = state) do
    %{state | tick: Process.send_after(self(), :tick, 1000)}
  end
  defp tick(%{tick: timer} = state) do
    Process.cancel_timer(timer)
    tick(%{state | tick: nil})
  end

  defp create_module(%{name: name, definitions: defs}) do
    funcs = defs_to_funcs(defs)

    #FIXME add functions to module, instead of recompiling it every time
    if :error != :code.get_object_code(name) do
      IO.warn "The context module already exists"
    end

    quote do
      defmodule unquote(name) do
        unquote_splicing(funcs)
      end
    end
    # |> IO.inspect(label: :generated_ast)
    |> inspect_ast(with_contexts: true, label: :generated)
    |> Code.compile_quoted("#{name}.ex")
    |> Enum.each(fn {module, binary} ->
      :code.load_binary(module, '#{Process.get :elixir_compiler_dest}/#{module}.beam', binary)
    end)
  end

  defp defs_to_funcs(defs) do
    Enum.flat_map(defs, fn {{module, name}, {_kind, clauses}} ->
      fname = fname(module, name)
      clauses = run(clauses)
      for {args, guards, body} <- clauses do
        args = ElixirTranslator.from_tria(args)
        guards = ElixirTranslator.from_tria(guards)
        body = ElixirTranslator.from_tria(body)
        case guards do
          [] ->
            quote do
              def(unquote(fname)(unquote_splicing args), unquote(body))
            end

          guards ->
            guards = Enum.reduce(guards, fn right, left -> {:when, [], [left, right]} end)
            quote do
              def(unquote(fname)(unquote_splicing args) when unquote(guards), unquote(body))
            end
        end
      end
    end)
  end

  # Transforms the `{args, guards, body}` to the `fn`, runs optimizers
  # and transforms the result back to `{args, guards, body}`

  defp run(clauses) do
    fn_clauses =
      Enum.flat_map(clauses, fn {args, guards, body} ->
        pattern = add_guards(args, guards)
        quote do
          unquote_splicing(pattern) -> try(unquote(body))
        end
      end)

    thefn = {:fn, [], fn_clauses}
    {:fn, _, fn_clauses} = Tria.run(thefn, translate: false)

    Enum.map(fn_clauses, fn
      {:"->", _, [pattern, {:try, _, [body]}]} ->
        {args, guards} = pop_guards(pattern)
        {args, guards, body}

      {:"->", _, [pattern, body]} ->
        {args, guards} = pop_guards(pattern)
        {args, guards, [do: body]}
    end)
  end

  # Guards helpers

  defp pop_guards(pattern) do
    case :lists.reverse(pattern) do
      [{:when, _, [arg, guards]} | tail] ->
        {:lists.reverse([arg | tail]), unjoin_guards(guards)}

      _ ->
        {pattern, []}
    end
  end

  defp add_guards(args, []), do: args
  defp add_guards(args, guards) do
    List.update_at(args, -1, fn last_arg ->
      quote do
        unquote(last_arg) when unquote(join_guards guards)
      end
    end)
  end

  defp join_guards([guard]), do: guard
  defp join_guards([guard | guards]) do
    {:when, [], [guard, join_guards(guards)]}
  end

  defp unjoin_guards({:when, _, [guard, guards]}), do: [guard | unjoin_guards guards]
  defp unjoin_guards(guard), do: [guard]

end
