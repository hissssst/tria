defmodule Tria.Son.Graph do
  @moduledoc """
  Two kinds of relations

  1. `v1 --argN----> v2` means that v2 is a first argument of v1
  2. `v1 --before--> v2` means that v1 must always be executed before v2

  Each relation is backtrackable. Transitive relations are not represented in the graph

  Each vertex can be
  1. Function (like Map.get) or basic operation (like cons, +)
  2. Control flow construction like `if`
  3. end, start and dead special value
  4. Literal

  ## Example

  Incomplete graph without START and END nodes for `123 + 456` would look like this:

  ```elixir
  %Tria.Son.Graph{
    c: 3,
    i: %{
      1 => {{Kernel, :+, 2}, %{{:arg, 1} => 2, {:arg, 2} => 3}},
      2 => {123, %{ {:back, {:arg, 1}} => 1 }},
      3 => {456, %{ {:back, {:arg, 2}} => 1 }}
    }
  }
  ```
  """

  defstruct [
    c: 1,
    i: %{
      end:   {:end, %{}},
      start: {:start, %{}}
    }
  ]

  ## Mutations

  def add_vertex(%__MODULE__{c: c, i: i} = g, node) do
    id = c + 1
    i = Map.put(i, id, {node, %{}})
    {id, %__MODULE__{g | c: id, i: i}}
  end

  # TODO check kind and from|to ids
  def add_edge(%__MODULE__{i: i} = g, from, to, kind) do
    case i do
      %{^from => {f, fe}, ^to => {t, te}} ->
        fe = Map.put(fe, kind, to)
        te = Map.update(te, {:back, kind}, [from], &[from | &1])
        %__MODULE__{g | i: %{i | from => {f, fe}, to => {t, te}}}

      _ ->
        raise "From or To are not found"
    end
  end

  def add_args_edges(%__MODULE__{i: i} = g, to_id, args) do
    {value, links} = Map.fetch!(i, to_id)
    if Map.has_key?(links, {:arg, 0}), do: raise "Already has args!"

    {i, links, _} =
      Enum.reduce(args, {i, links, 0}, fn arg_id, {i, links, arg_index} ->
        links = Map.put(links, {:arg, arg_index}, arg_id)
        i =
          Map.update!(i, arg_id, fn {value, links} ->
            {value, Map.update(links, {:back, {:arg, arg_index}}, [to_id], &[to_id | &1])}
          end)

        {i, links, arg_index + 1}
      end)

    %__MODULE__{g | i: Map.put(i, to_id, {value, links})}
  end

  def update_vertex(%__MODULE__{i: i} = g, id, update_function) do
    case i do
      %{^id => {value, edges}} ->
        %__MODULE__{g | i: %{i | id => {update_function.(value), edges}}}

      _ ->
        raise "Specified ID #{inspect id} is not present in graph"
    end
  end

  ### Joins two nodes into one node
  ### Drops the value of `from` in favor of `to` value
  def join(%__MODULE__{i: i} = g, from_id, to_id) do
    {_from_value, from_links} = Map.fetch!(i, from_id)
    {to_value, to_links} = Map.fetch!(i, to_id)

    new_links =
      Enum.reduce(from_links, %{}, fn
        {{:back, link}, ids}, acc ->
          ids = List.delete(ids, to_id)
          Map.put(acc, {:back, link}, ids)

        {_link, ^to_id}, acc ->
          acc

        {link, id}, acc ->
          Map.put(acc, link, id)
      end)

    new_links =
      Enum.reduce(to_links, new_links, fn
        {{:back, link} = key, ids}, acc ->
          case acc do
            %{^key => other_ids} ->
              ids = ids ++ (List.delete(other_ids, from_id) -- ids)
              Map.put(acc, {:back, link}, ids)

            _ ->
              ids = List.delete(ids, from_id)
              Map.put(acc, {:back, link}, ids)
          end

        {_link, ^to_id}, acc ->
          acc

        {link, id}, acc ->
          Map.put(acc, link, id)
      end)

    i =
      Enum.reduce(new_links, i, fn
        {{:back, link}, ids}, i ->
          Enum.reduce(ids, i, fn id, i ->
            Map.update!(i, id, fn {value, links} ->
             {value, Map.replace!(links, link, to_id)}
            end)
          end)

        {link, id}, i ->
          Map.update!(i, id, fn {value, links} ->
           {value, Map.update!(links, {:back, link}, fn ids -> ids -- [to_id, from_id] ++ [to_id] end)}
          end)
      end)

    i =
      i
      |> Map.replace!(to_id, {to_value, new_links})
      |> Map.delete(from_id)

    %__MODULE__{g | i: i}
  end

  def join_all(graph, [head | tail]) do
    Enum.reduce(tail, graph, fn id, graph -> join(graph, id, head) end)
  end

  ## Queries

  def value(%__MODULE__{i: i}, id) do
    %{^id => {v, _}} = i
    v
  end

  def args(%__MODULE__{i: i}, id) do
    {_value, links} = Map.fetch!(i, id)
    do_args(links, 0)
  end

  defp do_args(links, i) do
    key = {:arg, i}
    case links do
      %{^key => value} ->
        [value | do_args(links, i + 1)]

      _ ->
        []
    end
  end

  def backlinks(%__MODULE__{i: i}, id) do
    {_value, links} = Map.fetch!(i, id)
    Enum.flat_map(links, fn
      {{:back, link}, ids} ->
        Enum.map(ids, fn id -> {link, id} end)

      _ ->
        []
    end)
  end

  ## Inspection helpers

  def label({kind, pattern}) when kind in ~w[clause pattern structure_expression]a do
    string = escape Tria.Language.ast_to_string pattern
    "#{kind}: #{string}"
  end

  def label({:call, {m, f, a}}) do
    "#{inspect m}.#{f}/#{a}"
  end

  def label({:literal, literal}) do
    "literal: #{escape inspect literal}"
  end

  def label(:case), do: "case"
  def label(:end), do: "END"
  def label(:start), do: "START"

  def label(other) do
    inspect(other)
  end

  def arrow_label({:arg, n}) do
    "arg#{n}"
  end

  def arrow_label(other) do
    inspect(other)
  end

  defp escape(string) do
    String.replace(string, <<?">>, <<?\\, ?">>)
  end

  ## Visualization helpers

  def to_dot(%__MODULE__{i: i}) do
    fill =
      Enum.reduce(i, "", fn {id, {v, edges}}, acc ->
        label = label(v)
        {befores, args} =
          edges
          |> Enum.filter(fn
            {{:back, _}, _} -> false
            _ -> true
          end)
          |> Enum.split_with(fn
            {:before, _} -> true
            _ -> false
          end)

        [
          acc,
          "#{id} [label=\"#{label}\"]",
          Enum.map(args, fn {n, arg_id} -> ~s|#{id} -> #{arg_id} [label="#{arrow_label n}"]| end),
          Enum.map(befores, fn {_, before_id} -> "#{before_id} -> #{id} [style=dotted]" end)
        ]
        |> List.flatten()
        |> Enum.join("\n  ")
      end)

    """
    digraph sea {#{fill}
    }
    """
  end

  def visualize(%__MODULE__{} = graph) do
    IO.inspect graph
    dot = to_dot(graph)
    IO.puts dot
    System.shell("echo '#{dot}' | dot -T svg | imv -")
    :ok
  end

end
