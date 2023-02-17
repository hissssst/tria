defmodule Tria.Language.FunctionGraphTest do
  use ExUnit.Case, async: true

  alias Tria.Language.FunctionGraph

  setup do
    graph = :"#{__MODULE__}_#{:erlang.unique_integer [:positive]}"
    FunctionGraph.to_list(graph)
    {:ok, graph: graph}
  end

  test "set and get", %{graph: graph} do
    x = {Module, :x, 1}
    y = {Module, :y, 1}
    FunctionGraph.link(graph, x, y)
    assert FunctionGraph.linked?(graph, x, y)
    refute FunctionGraph.linked?(graph, y, x)
  end

  test "unlink", %{graph: graph} do
    x = {Module, :x, 1}
    y = {Module, :y, 1}
    FunctionGraph.link(graph, x, y)

    assert FunctionGraph.linked?(graph, x, y)
    refute FunctionGraph.linked?(graph, y, x)

    FunctionGraph.unlink(graph, x, y)

    refute FunctionGraph.linked?(graph, x, y)
    refute FunctionGraph.linked?(graph, y, x)
  end

  test "to_list", %{graph: graph} do
    x = {Module, :x, 1}
    y = {Module, :y, 1}
    z = {Module, :z, 1}

    FunctionGraph.link(graph, x, y)
    FunctionGraph.link(graph, x, z)
    FunctionGraph.link(graph, y, z)

    list =
      graph
      |> FunctionGraph.to_list()
      |> Enum.sort()

    assert list == [{x, y}, {x, z}, {y, z}]
  end

  test "link_many", %{graph: graph} do
    x = {Module, :x, 1}
    y = {Module, :y, 1}
    z = {Module, :z, 1}

    list = [{x, y}, {x, z}, {y, z}]

    FunctionGraph.link_many(graph, list)

    assert FunctionGraph.linked?(graph, x, y)
    assert FunctionGraph.linked?(graph, x, z)
    assert FunctionGraph.linked?(graph, y, z)

    refute FunctionGraph.linked?(graph, y, x)
    refute FunctionGraph.linked?(graph, z, x)
    refute FunctionGraph.linked?(graph, z, y)
  end

  test "unlink_rights", %{graph: graph} do
    f = {Module, :f, 1}
    g = {OtherModule, :g, 2}
    h = {AnotherModule, :h, 3}

    links = [
      {f, g},
      {f, h},
      {g, h}
    ]

    FunctionGraph.link_many(graph, links)
    FunctionGraph.unlink_rights(graph, f)

    assert [{^g, ^h}] = FunctionGraph.to_list(graph)
  end

  test "unlink_rights respect modules", %{graph: graph} do
    m = {Module, nil, 0}
    f = {Module, :f, 1}
    g = {OtherModule, :g, 2}
    h = {AnotherModule, :h, 3}

    links = [
      {f, g},
      {f, h},
      {g, h}
    ]

    FunctionGraph.link_many(graph, links)
    FunctionGraph.unlink_rights(graph, m)

    assert [{^g, ^h}] = FunctionGraph.to_list(graph)
  end

end
