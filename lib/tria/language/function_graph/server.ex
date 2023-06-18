defmodule Tria.Language.FunctionGraph.Server do
  @moduledoc """
  Server for managing existing graphs and their ets tables
  """

  use GenServer

  alias Tria.Language.FunctionGraph

  ## Public

  @spec start(Keyword.t()) :: pid()
  def start(opts \\ []) do
    case GenServer.start(__MODULE__, opts, name: __MODULE__) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  @spec delete_graph(FunctionGraph.t()) :: :ok | {:error, :not_found}
  def delete_graph(graph) do
    GenServer.call(start(), {:detele_graph, graph})
  end

  @spec new_graph(FunctionGraph.t(), Keyword.t()) :: :ets.tid()
  def new_graph(graph, opts) do
    GenServer.call(start(), {:new_graph, graph, opts})
  end

  @spec list_graphs() :: [FunctionGraph.t()]
  def list_graphs do
    GenServer.call(start(), :list_graphs)
  end

  ## GenServer callbacks

  def init(_opts) do
    state = %{graphs: MapSet.new()}
    {:ok, state}
  end

  def handle_call(:list_graphs, _from, %{graphs: graphs} = state) do
    {:reply, graphs, state}
  end

  def handle_call({:delete_graph, graph}, _from, %{graphs: graphs} = state) do
    if graph in graphs do
      :ets.delete(graph)
      {:reply, :ok, %{state | graphs: MapSet.delete(graphs, graph)}}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:new_graph, graph, opts}, _from, %{graphs: graphs} = state) do
    reference =
      case :ets.whereis(graph) do
        :undefined ->
          :ets.new(graph, [:bag, :public, :named_table, read_concurrency: true, write_concurrency: true])

        ref ->
          if graph not in graphs and not opts[:existing] do
            raise "Table #{graph} already exists"
          end
          ref
      end

    {:reply, reference, %{state | graphs: MapSet.put(graphs, graph)}}
  end
end
