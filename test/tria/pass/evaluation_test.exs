defmodule Tria.Pass.EvaluationTest do
  use ExUnit.Case

  import Tria.Tri
  import Tria.Common
  alias Tria.Pass.Evaluation

  describe "simple arithmetics" do
    test "constant" do
      evaluated =
        tri do
          x = 1 + 2
          y = x + x
          y + 5
        end
        |> Evaluation.run_once!(pure_functions: [{:"Elixir.Kernel", :"+", 2}])

      assert 11 == evaluated
    end

    test "x + y" do
      evaluated =
        tri do
          a = 1 + 2
          b = 2 + 2
          a + b + x + y
        end
        |> Evaluation.run_once!(pure_functions: [{:"Elixir.Kernel", :"+", 2}])

      assert tri(7 + x + y) = evaluated
    end
  end

  describe "matchers" do
    test "yes case by value" do
      evaluated =
        tri do
          x = 1
          case x do
            3 -> 3 + x
            2 -> 2 + x
            1 -> 1 + x
            other -> other
          end
        end
        |> Evaluation.run_once!(pure_functions: [{:"Elixir.Kernel", :"+", 2}])

      assert 2 == evaluated
    end

    test "yes case by pattern" do
      evaluated =
        tri do
          case {x, y} do
            {one, two, three} -> one + two + three
            {left, right} -> left + right
            {one} -> one
            other -> 0
          end
        end
        |> Evaluation.run_once!()

      assert tri(Kernel.+(_x, _y)) = evaluated
    end

    test "maybe case" do
      evaluated =
        tri do
          case [1, 2 | tail] do
            [] -> 0
            [_] -> 1
            [2, 2] -> :twos
            [_, _] -> 2
            other -> length(other)
          end
        end
        |> Evaluation.run_once!()

      assert(tri do
        case [1, 2 | _tail] do
          [_, _] -> 2
          other -> Kernel.length(other)
        end
      end = evaluated)
    end
  end

end
