defmodule Tria.Optimizer.Pass.EnumFusionTest do
  use ExUnit.Case, async: true
  import Tria.TestHelpers

  import Tria.Language, warn: false
  import Tria.Language.Tri
  import Tria.Language.Meta
  alias Tria.Optimizer.Pass.EnumFusion
  alias Tria.Compiler.AbstractTranslator

  defp run_while(tria) do
    EnumFusion.run_while tria
  end

  @tri_opts to_tria: :force, meta: false

  describe "`for` optimization" do
    test "to map" do
      abstract do
        for x <- [1, 2, 3, 4, 5], do: x + 1
      end
      |> AbstractTranslator.to_tria!(as_block: :true)
      |> run_while()
      |> assert_tri do
        Enum.map([1, 2, 3, 4, 5], fn x -> Kernel.+(x, 1) end)
      end
    end

    test "Do not convert to map when there's a condition" do
      abstract do
        for x <- [1, 2, 3, 4, 5], x > 3, do: x + 1
      end
      |> AbstractTranslator.to_tria!(as_block: :true)
      |> run_while()
      |> assert_tri do
        :lists.reverse Enum.reduce([1, 2, 3, 4, 5], [], _)
      end
    end
  end

  describe "Reducify" do
    test "sum" do
      tri do
        [1, 2, 3]
        |> Enum.sum()
      end
      |> run_while()
      |> assert_tri do
        Enum.reduce([1, 2, 3], 0, fn x, acc -> Kernel.+(x, acc) end)
      end
    end

    test "product" do
      tri do
        [1, 2, 3]
        |> Enum.product()
      end
      |> run_while()
      |> assert_tri do
        Enum.reduce([1, 2, 3], 1, fn x, acc -> Kernel.*(x, acc) end)
      end
    end

    test "count" do
      tri do
        [1, 2, 3]
        |> Enum.count()
      end
      |> run_while()
      |> assert_tri do
        Enum.reduce([1, 2, 3], 0, fn _, acc -> Kernel.+(acc, 1) end)
      end
    end

    test "frequencies" do
      tri do
        [1, 2, 3]
        |> Enum.frequencies()
      end
      |> run_while()
      |> assert_tri do
        Enum.reduce([1, 2, 3], %{}, fn key, acc -> Map.update(acc, key, 1, fn x -> Kernel.+(x, 1) end) end)
      end
    end
  end

  describe "Unreject" do
    test "In chain" do
      tri do
        Enum.reject(list, fn x -> x > 10 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.filter(list, fn x -> !(x > 10) end)
      end
    end
  end

  describe "Joining" do
    test "filter and filter, all pure" do
      tri do
        list
        |> Enum.filter(fn x -> x > 10 end)
        |> Enum.filter(fn x -> x < 20 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.filter(list, fn x -> x > 10 && x < 20 end)
      end
    end

    test "filter and filter, one pure" do
      tri do
        list
        |> Enum.filter(fn x -> send(self(), x) > 10 end)
        |> Enum.filter(fn x -> x < 20 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.filter(list, fn x -> send(self(), x) > 10 && x < 20 end)
      end
    end

    test "filter and filter, stream" do
      tri do
        list
        |> Stream.filter(fn x -> send(self(), x) > 10 end)
        |> Enum.filter(fn x -> send(self(), x) < 20 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.filter(list, fn x -> send(self(), x) > 10 && send(self(), x) < 20 end)
      end
    end

    test "impure not joined" do
      tri do
        list
        |> Enum.filter(fn x -> send(self(), x) > 10 end)
        |> Enum.filter(fn x -> send(self(), x) < 20 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        list
        |> Enum.filter(fn x1 -> send(self(), x1) > 10 end)
        |> Enum.filter(fn x2 -> send(self(), x2) < 20 end)
      end
    end

    test "map and map" do
      tri do
        list
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.map(fn x -> x * 2 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.map(list, fn x -> (x + 1) * 2 end)
      end
    end

    test "map and each" do
      tri do
        list
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.each(fn x -> x * 2 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.each(list, fn x -> (x + 1) * 2 end)
      end
    end

    test "flat_map and flat_map" do
      tri do
        list
        |> Enum.flat_map(fn x -> [x + 1] end)
        |> Enum.flat_map(fn x -> [x * 2] end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.flat_map(list, fn x -> Enum.flat_map([x + 1], fn y -> [y * 2] end) end)
      end
    end

    test "filter and each" do
      tri do
        list
        |> Enum.filter(fn x -> x > 10 end)
        |> Enum.each(fn x -> send(self(), x) end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.each(list, fn x -> if x > 10, do: send(self(), x) end)
      end
    end

    test "concat and reduce" do
      tri do
        list
        |> Enum.concat()
        |> Enum.reduce([], fn x, acc -> [x | acc] end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.reduce(list, [], fn x, acc ->
          Enum.reduce(x, acc, fn y, yacc -> [y | yacc] end)
        end)
      end
    end

    test "map and reduce" do
      tri do
        list
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.reduce([], fn x, acc -> [x | acc] end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.reduce(list, [], fn x, acc -> [x + 1 | acc] end)
      end
    end

    test "filter and reduce" do
      tri do
        list
        |> Enum.filter(fn x -> x > 0 end)
        |> Enum.reduce([], fn x, acc -> [x | acc] end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.reduce(list, [], fn x, acc -> if x > 0, do: [x | acc], else: acc end)
      end
    end

    test "flat_map and reduce" do
      tri do
        list
        |> Enum.flat_map(fn x -> [x + 1, x + 2, x + 3] end)
        |> Enum.reduce([], fn x, acc -> [x | acc] end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.reduce(list, [], fn x, acc ->
          Enum.reduce([x + 1, x + 2, x + 3], acc, fn y, yacc ->
            [y | yacc]
          end)
        end)
      end
    end
  end

  describe "to_list removed" do
    test "After map" do
      tri do
        list
        |> Enum.to_list()
        |> Enum.map(fn x -> x + 1 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.map(list, fn x -> x + 1 end)
      end
    end
  end

  describe "Enum.into map" do
    test "Into empty map without fn" do
      tri do
        Enum.into(list, %{})
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Map.new(list)
      end
    end

    test "Into empty map with fn" do
      tri do
        Enum.into(list, %{}, fn x -> {x, x} end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Map.new(list, fn x -> {x, x} end)
      end
    end

    test "With Enum.map empty" do
      tri do
        list
        |> Enum.map(fn x -> {x, x} end)
        |> Map.new()
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Map.new(list, fn x -> {x, x} end)
      end
    end

    test "With Enum.map" do
      tri do
        list
        |> Enum.map(fn x -> {x, x} end)
        |> Map.new(fn {x, x} -> {x - 1, x + 1} end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Map.new(list, fn x -> {x - 1, x + 1} end)
      end
    end

    test "Non-empty map not optimized" do
      tri do
        Enum.into(list, %{x: 1}, fn x -> {x, x} end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        Enum.into(list, %{x: 1}, fn x -> {x, x} end)
      end
    end
  end
end
