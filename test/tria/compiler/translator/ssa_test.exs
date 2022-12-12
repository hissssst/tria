defmodule Tria.Compiler.SSATranslatorTest do
  use ExUnit.Case

  import Tria.Language,
    only: [is_pinned: 1, inspect_ast: 2, is_variable: 1, prewalk: 2],
    warn: false
  import Tria.Language.Tri
  import Tria.TestHelpers

  alias Tria.Compiler.SSATranslator

  @tri_opts meta: false

  describe "Regression" do
    test "fn clause map match" do
      code =
        tri do
          x = 1
          fn %{old_x: ^x, x: x = _} when :erlang.is_atom(x) -> x end
        end
        |> SSATranslator.from_tria!()

      assert_tri code do
        x1 = 1
        fn %{old_x: ^x1, x: x2 = underscore} when :erlang.is_atom(x2) -> x2 end
      end

      assert 3 == length Enum.uniq [underscore, x1, x2]
    end
  end

  describe "Basics" do
    test "variable renaming" do
      code =
        tri do
          x = 1
          y = 2
          x = {x, y}
          y = {x, x}
        end
        |> SSATranslator.from_tria!()

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
        |> SSATranslator.from_tria!()

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
        |> SSATranslator.from_tria!()

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
        |> SSATranslator.from_tria!()

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
        |> SSATranslator.from_tria!()

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
        |> SSATranslator.from_tria!()

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
      tri do
        x = 1
        s = <<something :: binary-size(x)>>
        <<x :: binary-size(x), x :: binary-size(1)>> = s
      end
      |> SSATranslator.from_tria!()
      |> assert_tri do
        x1 = 1
        s = <<something :: binary-size(x1)>>
        <<x2 :: binary-size(x1), x2 :: binary-size(1)>> = s
      end

      assert 4 == length Enum.uniq [x1, x2, s, something]
    end

    test "Defined and than used" do
      tri do
        x = 0
        <<x :: 8, y :: binary-size(x)>> = <<1, 2>>
      end
      |> SSATranslator.from_tria!()
      |> assert_tri do
        x1 = 0
        <<x2 :: 8, y :: tri {:-, _, [{:binary, _, _}, {:size, _, [x2]}]}>> = <<1, 2>>
      end

      assert 3 == length Enum.uniq [y, x1, x2]
    end

    test "Tricky" do
      tri do
        binary = 1
        <<x :: 8, y :: binary-size(8)>> = <<1, 2>>
      end
      |> SSATranslator.from_tria!()
      |> assert_tri do
        binary1 = 1
        <<x :: 8, y :: binary-size(8)>> = <<1, 2>>
      end

      assert is_variable binary1
      assert_unique [x, y, binary1]
    end
  end

  describe "Pin known" do
    test "Simple" do
      tri do
        x = 0
        x = 1
      end
      |> SSATranslator.from_tria!(pin_known: true)
      |> assert_tri do
        x = 0
        ^x = 1
      end
    end

    test "Reused" do
      tri do
        x = 0
        x = 1
        y = [x]
      end
      |> SSATranslator.from_tria!(pin_known: true)
      |> assert_tri do
        x = 0
        ^x = 1
        y = [x]
      end

      assert x != y
    end

    test "Different clauses" do
      tri do
        case x do
          :first_clause ->
            x = 0
            x = 1

          :second_clause ->
            x = 2
        end
      end
      |> SSATranslator.from_tria!(pin_known: true)
      |> assert_tri do
        case x1 do
          :first_clause ->
            x2 = 0
            ^x2 = 1

          :second_clause ->
            x3 = 2
        end
      end

      assert Enum.all?([x1, x2, x3], &is_variable/1)
      assert 3 == length Enum.uniq [x1, x2, x3]
    end

    test "Regression case" do
      tri do
        case something do
          x when x == false when x == nil ->
            x

          _ ->
            :other
        end
      end
      |> SSATranslator.from_tria!(pin_known: true)
      |> assert_tri do
        case something do
          x when Kernel.==(x, false) when Kernel.==(x, nil) ->
            x

          _ ->
            :other
        end
      end

      assert not is_pinned x
      assert x != something
    end
  end
end
