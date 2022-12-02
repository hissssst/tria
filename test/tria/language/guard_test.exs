defmodule Tria.Langauge.GuardTest do
  use ExUnit.Case, async: true

  import Tria.Language, only: [inspect_ast: 2], warn: false
  import Tria.Language.Guard
  import Tria.Compiler.ElixirTranslator, only: [to_tria!: 2]

  describe "is_guard/1" do
    test "Truthy" do
      result =
        quote do
          is_atom(a) and is_integer(i)
          or (is_list(l) or is_map(m))
          and length(l) == 4
          and tuple_size(t) == 3
          and map_size(m) == i
          and i > 0
          and "x" <> "y" == "xy"
          and :erlang.map_get(m, 8 * (i - 1)) == :x
        end
        |> to_tria!(%{__ENV__ | context: :guard})
        |> is_guard()

      assert true == result
    end

    test "False function" do
      result =
        quote do
          is_atom(a) and is_integer(i)
          or (is_list(l) or is_map(m))
          and length(l) == 4
          and tuple_size(t) == 3
          and map_size(m) == i
          and i > 0
          and function(x, y, z)
        end
        |> to_tria!(%{__ENV__ | context: :guard})
        |> is_guard()

      assert false == result
    end

    test "False matching" do
      result =
        quote do
          is_atom(a) and is_integer(i)
          or (is_list(l) or is_map(m))
          and length(l) == 4
          and tuple_size(t) == 3
          and map_size(m) == i
          and i > 0
          and (x = 1)
        end
        |> to_tria!(%{__ENV__ | context: :guard})
        |> is_guard()

      assert false == result
    end
  end
end
