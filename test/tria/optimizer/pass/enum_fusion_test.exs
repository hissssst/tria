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
        list
        |> Enum.map(fn x -> Kernel.send(pid, x) end)
        |> Enum.reject(fn x -> x > 10 end)
      end
      |> run_while()
      |> unmeta()
      |> assert_tri do
        list
        |> Enum.map(fn x2 -> Kernel.send(pid, x2) end)
        |> Enum.filter(fn x1 -> !(x1 > 10) end)
      end
    end
  end

  describe "Joining" do

  end
end
