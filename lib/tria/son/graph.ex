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
    c: 2,
    i: %{
      end:   {:end, %{}},
      start: {:start, %{}},
      dead:  {:dead, %{}}
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
        te = Map.put(te, {:back, kind}, from)
        %__MODULE__{g | i: %{i | from => {f, fe}, to => {t, te}}}

      _ ->
        raise "From or To are not found"
    end
  end

  def update_vertex(%__MODULE__{i: i} = g, id, update_function) do
    case i do
      %{^id => {value, edges}} ->
        %__MODULE__{g | i: %{i | id => {update_function.(value), edges}}}

      _ ->
        raise "Specified ID #{inspect id} is not present in graph"
    end
  end

  ## Queries

  def value(%__MODULE__{i: i}, id) do
    %{^id => {v, _}} = i
    v
  end

  def args(%__MODULE__{i: i}, id) do
    %{^id => {_, edges}} = i
    args =
      Enum.reduce(edges, %{}, fn
        {{:arg, i}, v}, acc -> Map.put(acc, i, v)
        _, acc -> acc
      end)

    Enum.map(0..(map_size(args) - 1), fn i -> :erlang.map_get(i, args) end)
  end

  ## Inspection helpers

  def label({:pattern, pattern}) do
    string =
      pattern
      |> Tria.Language.ast_to_string()
      |> String.replace(<<?">>, <<?\\, ?">>)

    "pattern: #{string}"
  end

  def label(other) do
    inspect(other)
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
          Enum.map(args, fn {n, arg_id} -> ~s|#{id} -> #{arg_id} [label="#{inspect n}"]| end),
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
