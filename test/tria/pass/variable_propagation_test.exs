defmodule Tria.Pass.VariablePropagationTest do
  use ExUnit.Case, async: true
  import Tria.Matcher

  alias Tria.Pass.VariablePropagation

  describe "Simple clauses" do
    test "a + b" do
      assert tri(1 + _) =
               quote(do: a + b)
               |> VariablePropagation.run_once([{{:a, __MODULE__}, 1}])
               |> elem(0)

      assert tri(_ + 2) =
               quote(do: a + b)
               |> VariablePropagation.run_once([{{:b, __MODULE__}, 2}])
               |> elem(0)

      assert tri(1 + 2) =
               quote(do: a + b)
               |> VariablePropagation.run_once([{{:a, __MODULE__}, 1}, {{:b, __MODULE__}, 2}])
               |> elem(0)
    end

    test "pattern" do
      quoted =
        quote do
          [%{xyz: {^x, y, z}} | _] =
            (
              x = 0
              x + y + z
            )

          x + y + z
        end

      assert (tri do
                [%{xyz: {1, tri(:y, _, _), tri(:z, _, _)}} | _] =
                  (
                    x = 0
                    x + 2 + 3
                  )

                tri(:x, _, _) + tri(:y, _, _) + tri(:z, _, _)
              end) =
               quoted
               |> VariablePropagation.run_once([
                 {{:x, __MODULE__}, 1},
                 {{:y, __MODULE__}, 2},
                 {{:z, __MODULE__}, 3}
               ])
               |> elem(0)
               |> Tria.Common.inspect_ast()
    end
  end

  describe "Not so simple clauses" do
    test "Case and equals" do
      result =
        quote do
          z = x + y

          case z do
            3 -> x
            ^x -> z
            x -> x + y
          end
        end
        |> VariablePropagation.run_once([
          {{:x, __MODULE__}, 1},
          {{:y, __MODULE__}, 2},
          {{:z, __MODULE__}, 333}
        ])
        |> elem(0)

      assert (tri do
                z = 1 + 2

                case z do
                  3 -> 1
                  1 -> z
                  x -> x + 2
                end
              end) = result
    end
  end

  describe "Context intersection" do
    test "simple" do
      result =
        quote do
          y = 1
          a = x
          b = x

          {a, b, y}
        end
        |> VariablePropagation.run_once([
          {{:x, __MODULE__},
           quote(
             do:
               (
                 y = 1000
                 y + 1
               )
           )}
        ])
        |> elem(0)
        |> Tria.Common.inspect_ast()
        |> Code.eval_quoted()
        |> elem(0)

      assert {1001, 1001, 1} = result
    end
  end
end
