defmodule Tria.Language.FunctionGraph do

  @moduledoc """
  Helper module for manipulating graphs of functions.
  These graphs are directed and weightless.
  Graphs are structured to be efficient in forward vertex child traversal.
  Backtracking is supported, but optional for some functions

  > Note:
  >
  > `{Module, nil, 0}` mfarity is considered a stub mfarity which
  > defines that every function of the `Module` is present in the graph
  """

  alias Tria.Language.MFArity
  alias Tria.Language.FunctionGraph.Server
  import Tria.Language.MFArity, only: [is_mfarity: 1]

  defguard are_mfarity(left, right) when is_mfarity(left) and is_mfarity(right)

  @typedoc "Graph's name is an Atom"
  @type t :: atom()

  @type vertex :: MFArity.t()

  @type link :: {vertex(), vertex()}

  @type links :: [link()]

  ## Public functions

  @doc """
  Returns a list of all created and not yet deleted graphs.
  Note that graphs without links are not deleted automatically and
  will be returned to this function
  """
  @spec all_graphs() :: [t()]
  def all_graphs do
    Server.list_graphs()
  end

  @doc """
  Loads existing ets table as graph
  """
  @spec load(t()) :: :ok
  def load(graph) do
    ensure_graph(graph, existing: true)
    :ok
  end

  @doc """
  Lists all vertexes reachable from passed `vertex` in any amount of steps.
  """
  @spec reachable(t(), vertex()) :: [vertex()]
  def reachable(graph, vertex) do
    graph
    |> ensure_graph()
    |> do_reachable(vertex, %{})
    |> Map.keys()
  end

  @doc """
  Links one vertex to another in graph.
  """
  @spec link(t(), vertex(), vertex(), Keyword.t()) :: :ok
  def link(graph, left, right, opts \\ []) when are_mfarity(left, right) do
    graph
    |> ensure_graph()
    |> do_link(left, right, opts)

    :ok
  end

  @doc """
  Lists all vertexes directly linked from this vertex.
  Doesn't respect modules.
  """
  @spec links(t(), vertex()) :: [vertex()]
  def links(graph, vertex) do
    graph
    |> ensure_graph()
    |> do_links(vertex)
  end

  @doc """
  Removes specified link, returning `true` if link was present.
  Doesn't respect modules.
  """
  @spec unlink(t(), vertex(), vertex()) :: boolean()
  def unlink(graph, left, right) when are_mfarity(left, right) do
    ms = [
      {{{:"$1", :"$2"}, :"$3"},
       [
         {:orelse,
          {:andalso, {:andalso, {:==, :"$1", :forward}, {:==, :"$2", {:const, left}}},
           {:==, :"$3", {:const, right}}},
          {:andalso, {:andalso, {:==, :"$1", :backward}, {:==, :"$2", {:const, right}}},
           {:==, :"$3", {:const, left}}}}
       ], [true]}
    ]

    graph
    |> ensure_graph()
    |> :ets.select_delete(ms)
    |> Kernel.>(0)
  end

  @doc """
  Removes all links where the passed vertex is on the left side.
  Returns `true` when deleted any. Respects modules.
  """
  @spec unlink_rights(t(), left :: vertex()) :: boolean()
  def unlink_rights(graph, {module, nil, 0}) do
    ms = [
      {{{:"$1", {:"$2", :_, :_}}, {:"$3", :_, :_}},
       [
         {:orelse,
           { :andalso, {:==, :"$1", :forward},  {:==, :"$2", {:const, module}} },
           { :andalso, {:==, :"$1", :backward}, {:==, :"$3", {:const, module}} }
         }
       ], [true]}
    ]

    graph
    |> ensure_graph()
    |> :ets.select_delete(ms)
    |> Kernel.>(0)
  end
  def unlink_rights(graph, left) do
    ms = [
      {{{:"$1", :"$2"}, :"$3"},
       [
         {:orelse,
           { :andalso, {:==, :"$1", :forward},  {:==, :"$2", {:const, left}} },
           { :andalso, {:==, :"$1", :backward}, {:==, :"$3", {:const, left}} }
         }
       ], [true]}
    ]

    graph
    |> ensure_graph()
    |> :ets.select_delete(ms)
    |> Kernel.>(0)
  end

  @doc """
  Links left to right and also right to left
  """
  @spec bilink(t(), vertex(), vertex(), Keyword.t()) :: :ok
  def bilink(graph, left, right, opts \\ []) when are_mfarity(left, right) do
    graph = ensure_graph(graph)
    do_link(graph, left, right, opts)
    do_link(graph, right, left, opts)

    :ok
  end

  @doc """
  Checks if link from left to right exists. Doesn't respect modules
  """
  @spec linked?(t(), vertex(), vertex()) :: boolean()
  def linked?(graph, left, right) when are_mfarity(left, right) do
    graph
    |> ensure_graph()
    |> do_linked?(left, right)
  end

  @doc """
  Checks if link from left to right and from right to left exists.
  Doesn't respect modules.
  """
  @spec bilinked?(t(), vertex(), vertex()) :: boolean()
  def bilinked?(graph, left, right) when are_mfarity(left, right) do
    graph = ensure_graph(graph)
    do_linked?(graph, left, right) and do_linked?(graph, right, left)
  end

  @doc """
  Instantiates a list with all links from the graph as is.
  """
  @spec to_list(t()) :: [{vertex(), vertex()}]
  def to_list(graph) do
    graph
    |> ensure_graph()
    |> :ets.select([{{{:forward, :"$1"}, :"$2"}, [], [{{:"$1", :"$2"}}]}])
  end

  @doc """
  Instantiates a map with all lefts as keys and all rights in a value list.
  """
  @spec to_map(t()) :: %{vertex() => [vertex()]}
  def to_map(graph) do
    graph
    |> to_list()
    |> Enum.reduce(%{}, fn {key, value}, acc ->
      Map.update(acc, key, [value], &[value | &1])
    end)
  end

  @doc """
  Puts new links to specified graph
  """
  @spec link_many(t(), links(), Keyword.t()) :: :ok
  def link_many(graph, links, opts \\ []) do
    links = Enum.map(links, fn {left, right} -> {{:forward, left}, right} end)
    links =
      if opts[:backtrack] do
        links ++ Enum.map(links, fn {{_, left}, right} -> {{:backward, right}, left} end)
      else
        links
      end

    graph
    |> ensure_graph()
    |> :ets.insert(links)

    :ok
  end

  @doc """
  Puts new links to specified graph, removing the old ones from lefts
  """
  @spec relink_many(t(), links(), Keyword.t()) :: :ok
  def relink_many(graph, links, opts \\ []) do
    {lefts, links} =
      links
      |> Enum.map(fn {left, right} -> {left, {{:forward, left}, right}} end)
      |> Enum.unzip()

    lefts
    |> Enum.uniq()
    |> Enum.each(&unlink_rights(graph, &1))

    links =
      if opts[:backtrack] do
        links ++ Enum.map(links, fn {{_, left}, right} -> {{:backward, right}, left} end)
      else
        links
      end

    graph
    |> ensure_graph()
    |> :ets.insert(links)

    :ok
  end

  @doc """
  Completely deletes existing graphs with all it's data.
  """
  @spec delete_graph(t()) :: :ok | {:error, :not_found}
  def delete_graph(graph) do
    Server.delete_graph(graph)
  end

  ## Private functions

  defp do_reachable(_, vertex, accmap) when :erlang.map_get(vertex, accmap) do
    accmap
  end
  defp do_reachable(graph, vertex, accmap) do
    links = do_links(graph, vertex)
    Enum.reduce(links, accmap, fn linked_vertex, accmap ->
      do_reachable(graph, linked_vertex, Map.put(accmap, linked_vertex, []))
    end)
  end

  defp do_links(graph, vertex) do
    ms = [{ {{:forward, {:const, vertex}}, :"$1"}, [], [{:"$1"}] }]
    :ets.select(graph, ms)
  end

  defp do_link(graph, left, right, []) do
    :ets.insert(graph, {{:forward, left}, right})
  end
  defp do_link(graph, left, right, opts) do
    if Keyword.get(opts, :backtrack, true) do
      :ets.insert(graph, [{{:forward, left}, right}, {{:backward, right}, left}])
    else
      :ets.insert(graph, {{:forward, left}, right})
    end
  end

  defp do_linked?(graph, left, right) do
    :ets.match(graph, {{:forward, left}, right}) != []
  end

  def ensure_graph(graph, opts \\ []) when is_atom(graph) do
    with :undefined <- :ets.whereis(graph) do
      Server.new_graph(graph, opts)
    end
  end
end
