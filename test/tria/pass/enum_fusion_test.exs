defmodule Tria.Pass.EnumFusionTest do
  use ExUnit.Case

  import Tria.Common
  import Tria.Tri

  alias Tria.Pass.EnumFusion, as: EF

  defp cleanup_ast(ast), do: Macro.prewalk(ast, &remove_line/1)

  defp remove_line(node), do: Macro.update_meta(node, &Keyword.delete(&1, :line))

  describe "basic fusion" do
    test "two map calls" do
      src = tri do
        [1,2,3]
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.map(fn y -> y * 2 end)
      end
      |> cleanup_ast()

      fused = tri do
        [1,2,3]
        |> Enum.map(fn arg -> arg |> (fn x -> x + 1 end).() |> (fn y -> y * 2 end).() end)
      end
      |> cleanup_ast()

      assert EF.run(src) == fused
    end

    test "three map calls" do
      src = tri do
        [1,2,3]
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.map(fn y -> y * 2 end)
        |> Enum.map(fn z -> z - 3 end)
      end
      |> cleanup_ast()

      fused = tri do
        [1,2,3]
        |> Enum.map(fn arg -> arg |> (fn arg -> arg |> (fn x -> x + 1 end).() |> (fn y -> y * 2 end).() end).() |> (fn z -> z - 3 end).() end)
      end
      |> cleanup_ast()

      assert EF.run(src) == fused
    end

    test "four map calls" do
      src = tri do
        [1,2,3]
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.map(fn y -> y * 2 end)
        |> Enum.map(fn z -> z - 3 end)
        |> Enum.map(fn w -> w / 4 end)
      end
      |> cleanup_ast()

      fused = tri do
        [1,2,3]
        |> Enum.map(fn arg -> arg |> (fn arg -> arg |> (fn arg -> arg |> (fn x -> x + 1 end).() |> (fn y -> y * 2 end).() end).() |> (fn z -> z - 3 end).() end).() |> (fn w -> w / 4 end).() end)
      end
      |> cleanup_ast()

      assert EF.run(src) == fused
    end
  end

  describe "nested fusion" do
    test "two times" do
      src = tri do
        [[1,2,3]]
        |> Enum.map(fn x -> x |> Enum.map(fn x1 -> x1 + 1 end) |> Enum.map(fn x2 -> x2 + 2 end) end)
        |> Enum.map(fn y -> y |> Enum.map(fn y1 -> y1 * 1 end) |> Enum.map(fn y2 -> y2 * 2 end) end)
      end
      |> cleanup_ast()

      fused = tri do
        [[1,2,3]]
        |> Enum.map(fn arg -> arg |> (fn x -> x |> Enum.map(fn arg -> arg |> (fn x1 -> x1 + 1 end).() |> (fn x2 -> x2 + 2 end).() end) end).() |> (fn y -> y |> Enum.map(fn arg -> arg |> (fn y1 -> y1 * 1 end).() |> (fn y2 -> y2 * 2 end).() end) end).() end)
      end
      |> cleanup_ast()

      assert EF.run(src) == fused
    end
  end
end
