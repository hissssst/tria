defmodule Tria.Language.TriTest do
  use ExUnit.Case
  import Tria.Language
  import Tria.Language.Tri

  test "AST pattern" do
    quoted =
      quote do
        1 + 2
      end

    tri to_tria: false do
      x + 2
    end = quoted

    assert x == 1
  end

  test "`case` with `when`" do
    quoted = quote do: 1 + 2

    x =
      case quoted do
        tri([to_tria: false], x + 2) when is_integer(x) ->
          x + 3
      end

    assert x == 4
  end

  test "basic expression" do
    quoted = quote do: function x
    assert tri(function x) = quoted
    assert is_variable(x)
  end

  describe "tri in tri" do
    test "variable meta context" do
      quoted =
        quote do
          function(x)
        end

      tri do
        function(tri(:x, meta, ctx))
      end = quoted

      assert is_atom(ctx)
      assert is_list(meta)
    end
  end

  describe "tri as quote" do
    test "just works" do
      x = 1
      y = 2
      q =
        tri to_tria: false do
          x + y
        end
      assert {:+, _, [1, 2]} = q
    end

    test "partly isolates" do
      x = 1
      q =
        tri to_tria: false do
          x + y
        end
      assert {:+, _, [1, {:y, _, _}]} = q
    end

    test "isolates" do
      x = 1
      q =
        tri to_tria: false, isolate: true do
          x + y
        end

      assert {:+, _, [{:x, _, ctx}, {:y, _, ctx}]} = q
      assert x == 1 # To disable unused warning
    end
  end
end
