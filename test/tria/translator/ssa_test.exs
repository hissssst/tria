defmodule Tria.Translator.SSATest do
  use ExUnit.Case

  import Tria.Tri
  import Tria.Common, only: [inspect_ast: 2], warn: false
  alias Tria.Translator.SSA

  import Tria.TestHelpers

  @tri_opts meta: false

  describe "Basics" do
    test "variable renaming" do
      code =
        tri do
          x = 1
          y = 2
          x = {x, y}
          y = {x, x}
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = 1
        y1 = 2
        x2 = {x1, y1}
        y2 = {x2, x2}
      end

      assert x1 != x2
      assert y1 != y2
    end

    test "equation" do
      code =
        tri do
          x = 0
          x = (x = 1; x = 2; 3)
          x
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = 0
        x4 = (x2 = 1; x3 = 2; 3)
        x4
      end

      assert 4 == length Enum.uniq [x1, x2, x3, x4]
    end

    test "duplicates in pattern" do
      code =
        tri do
          {x, x, [x, x]} = x
        end
        |> SSA.from_tria()

      assert_tri code do
        {x2, x2, [x2, x2]} = x1
      end

      assert x1 != x2
    end
  end

  describe "Clauses" do
    test "case" do
      code =
        tri do
          x = y
          case (x = z; x) do
            %{key: x} -> x
            %{other_key: ^x} -> x
            _ -> {x, y}
          end
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = y
        case (x2 = z; x2) do
          %{key: x3} -> x3
          %{other_key: ^x2} -> x2
          _ -> {x2, y}
        end
      end

      assert 5 == length Enum.uniq [x1, x2, x3, y, z]
    end

    test "fn" do
      code =
        tri do
          x = y
          (fn
            %{key: x} -> x
            %{other_key: ^x} -> x
            _ -> {x, y}
          end).( (x = 2; x) )
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = y
        (fn
          %{key: x2} -> x2
          %{other_key: ^x1} -> x1
          _ -> {x1, y}
        end).( (x3 = 2; x3) )
      end

      assert 4 == length Enum.uniq [x1, x2, x3, y]
    end

    test "with" do
      code =
        tri do
          x = y
          with(
            x <- (x = z; x),
            x <- x
          ) do
            x
          else
            x -> x
            _ -> x
          end
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = y
        with(
          x3 <- (x2 = z; x2),
          x4 <- x3
        ) do
          x4
        else
          x5 -> x5
          _ -> x1
        end
      end

      assert 7 == length Enum.uniq [x1, x2, x3, x4, x5, y, z]
    end
  end

  describe "Binary" do
    test "constructing and matching" do
      code =
        tri do
          x = 1
          s = <<something :: binary-size(x)>>
          <<x :: binary-size(x), x :: binary-size(1)>> = s
        end
        |> SSA.from_tria()
        |> inspect_ast(label: :result, with_contexts: true)

      assert_tri code do
        x1 = 1
        s = <<something :: binary-size(x1)>>
        <<x2 :: binary-size(x1), x2 :: binary-size(1)>> = s
      end

      assert 4 == length Enum.uniq [x1, x2, s, something]
    end

    test "Defined and than used" do
      code =
        tri do
          x = 0
          <<x :: 8, y :: binary-size(x)>> = <<1, 2>>
        end
        |> SSA.from_tria()

      assert_tri code do
        x1 = 0
        <<x2 :: 8, y :: binary-size(x2)>> = <<1, 2>>
      end

      assert 3 == length Enum.uniq [y, x1, x2]
    end
  end
end
