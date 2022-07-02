defmodule Tria.MatcherTest do
  use ExUnit.Case
  import Tria.Common
  import Tria.Matcher

  test "AST pattern" do
    quoted =
      quote do
        1 + 2
      end

    tri(do: x + 2) = quoted

    assert x == 1
  end

  test "`case` with `when`" do
    quoted =
      quote do
        1 + 2
      end

    x =
      case quoted do
        tri(do: x + 2) when is_map(x) ->
          x + 4

        tri(do: x + 2) when is_integer(x) ->
          x + 3
      end

    assert x == 4
  end

  test "variable matching when" do
    quoted =
      quote do
        function(x)
      end

    x =
      case quoted do
        tri(do: function(x)) when is_variable(x) ->
          x
      end

    assert is_variable(x)
  end

  describe "tri" do
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
end
