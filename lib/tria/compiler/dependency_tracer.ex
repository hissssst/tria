defmodule Tria.Compiler.DependencyTracer do
  @moduledoc """
  Elixir's compiler tracer which traces inter-module dependencies.
  It works as a singleton process
  """

  alias Tria.Debug
  alias Tria.Language.FunctionGraph

  use GenServer

  @type graphs :: %{FunctionGraph.t() => FunctionGraph.links()}

  ## Public API

  @spec with_tracer((() -> result)) :: {result, graphs()}
        when result: any()
  def with_tracer(func) do
    start_link()
    try do
      {func.(), complete_all()}
    after
      pid = Process.whereis(__MODULE__)
      pid && Process.exit(pid, :kill)
    end
  end

  @spec start_link(ignored_opts :: Keyword.t()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec get_graphs() :: graphs()
  def get_graphs do
    GenServer.call(__MODULE__, :get_graphs)
  end

  @spec complete_all() :: graphs()
  def complete_all do
    GenServer.call(__MODULE__, :complete_all)
  end

  @spec complete_file(Path.t()) :: :ok
  def complete_file(file) do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) ->
        GenServer.call(pid, {:complete_file, file})

      _ ->
        :ok
    end
  end

  ## GenServer callbacks

  def init(_opts) do
    state = %{
      file_dependants: %{}, # file  -> [referenced_module_in_file]
      links: %{},           # graph -> [{mfarity, mfarity}]
      file_to_modules: %{}  # file  -> [module_defined_in_file]
    }

    {:ok, state}
  end

  def handle_cast({:link, graph, left, right}, %{links: links} = state) do
    links = do_link(links, graph, left, right)
    {:noreply, %{state | links: links}}
  end

  def handle_cast({:link_to_file, graph, mfarity, file}, %{file_dependants: file_dependants} = state) do
    pair = {graph, mfarity}
    file_dependants = Map.update(file_dependants, file, [pair], &[pair | &1])
    {:noreply, %{state | file_dependants: file_dependants}}
  end

  def handle_cast({:module_in_file, file, module}, %{file_to_modules: file_to_modules} = state) do
    file_to_modules = Map.update(file_to_modules, file, [module], & [module | &1])
    {:noreply, %{state | file_to_modules: file_to_modules}}
  end

  def handle_call({:complete_file, file}, _from, %{file_dependants: file_dependants, links: links, file_to_modules: file_to_modules} = state) do
    state =
      with(
        {deps, file_dependants} when is_list(deps) <- Map.pop(file_dependants, file),
        {modules, file_to_modules} when is_list(modules) <- Map.pop(file_to_modules, file)
      ) do
        links =
          for module <- modules, {graph, dep} <- deps, reduce: links do
            links -> do_link(links, graph, {module, nil, 0}, dep)
          end

        %{state | file_dependants: file_dependants, file_to_modules: file_to_modules, links: links}
      else
        _ -> state
      end

    {:reply, :ok, %{state | file_dependants: file_dependants}}
  end

  def handle_call(:complete_all, from, %{links: links} = state) do
    GenServer.reply(from, links)
    {:stop, :normal, state}
  end

  def handle_call(:get_graphs, _from, %{links: links} = state) do
    {:reply, links, state}
  end

  defp do_link(links, graph, left, right) do
    case links do
      %{^graph => pairs} -> %{links | graph => [{left, right} | pairs]}
      %{} -> Map.put(links, graph, [{left, right}])
    end
  end

  ## Elixir compiler tracing interface

  def trace({:require, _meta, required, _}, %Macro.Env{module: module}) do
    Debug.inspect({module, required}, label: :require_link)
    link_pair(:depends, {module, nil, 0}, {required, nil, 0})
  end

  def trace({macro, _, module, function, arity}, env) when macro in ~w[remote_macro imported_macro]a do
    link_to_current(:depends, {module, function, arity}, env)
  end

  def trace({:struct_expansion, _meta, module, []}, env) do
    link_to_current(:depends, {module, :__struct__, 0}, env)
  end

  def trace({:struct_expansion, _meta, module, _keys}, env) do
    link_to_current(:depends, {module, :__struct__, 1}, env)
  end

  def trace({call, _, module, function, arity}, env) when call in ~w[imported_function remote_function]a do
    link_to_current(:calls, {module, function, arity}, env)
  end

  def trace({:on_module, _, _}, %Macro.Env{module: module, file: file}) do
    GenServer.cast(__MODULE__, {:module_in_file, file, module})
  end

  def trace({:alias_reference, _, referenced}, %Macro.Env{file: file, function: nil}) do
    GenServer.cast(__MODULE__, {:link_to_file, :depends, {referenced, nil, 0}, file})
  end

  def trace(_event, _env), do: :ok

  # For calls in body of function
  defp link_to_current(graph, mfarity, %Macro.Env{module: module, function: {function, arity}}) do
    link_pair(graph, {module, function, arity}, mfarity)
  end

  # For calls in body of module, but not in function
  # `graphs` is ignore, since any call during compilation defines compile-time dependency
  defp link_to_current(_graph, mfarity, %Macro.Env{module: module}) do
    link_pair(:depends, {module, nil, 0}, mfarity)
  end
  defp link_to_current(_, _, _), do: :ok

  defp link_pair(graph, left, right) do
    Debug.inspect({graph, left, right}, label: :dependency_tracer_links)
    GenServer.cast(__MODULE__, {:link, graph, left, right})
  end
end
