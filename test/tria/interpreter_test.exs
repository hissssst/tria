defmodule Tria.InterpreterTest do
  use ExUnit.Case, async: true
  alias Tria.Interpreter

  describe "Matching" do
    test "simple yes" do
      pattern = quote(do: [1, 2, _])
      data = quote(do: [1, 2, 3])

      assert {:yes, _} = Interpreter.match?(pattern, data)
    end

    test "simple maybe" do
      pattern = quote(do: [1, 2, 3])
      data = quote(do: [1, 2, x])

      assert {:maybe, _} = Interpreter.match?(pattern, data)
    end

    test "simple no" do
      pattern = quote(do: [_, _, 3])
      data = quote(do: [1, 2, 4])

      assert :no = Interpreter.match?(pattern, data)
    end

    test "lists" do
      pattern = quote(do: [1, 2, 3 | _])
      data1 = quote(do: [1, 2, 3, 4, 5, 6])
      data2 = quote(do: [1, 2, 3])
      data3 = quote(do: [1, 2])
      data4 = quote(do: [1, 2 | foo()])
      data5 = quote(do: [1, 2, 2 | foo()])

      assert {:yes, _} = Interpreter.match?(pattern, data1)
      assert {:yes, _} = Interpreter.match?(pattern, data2)
      assert :no = Interpreter.match?(pattern, data3)
      assert {:maybe, _} = Interpreter.match?(pattern, data4)
      assert :no = Interpreter.match?(pattern, data5)
    end

    test "tuples" do
      pattern1 = quote(do: {_, 2})
      data1 = quote(do: {1, 2})
      data2 = quote(do: {2, 3})
      data3 = quote(do: {foo()})
      data4 = quote(do: {3, 2})
      data5 = quote(do: {3, foo()})
      data6 = quote(do: {3, []})
      data7 = quote(do: {foo(), 2})

      assert {:yes, _} = Interpreter.match?(pattern1, data1)
      assert :no = Interpreter.match?(pattern1, data2)
      assert :no = Interpreter.match?(pattern1, data3)
      assert {:yes, _} = Interpreter.match?(pattern1, data4)
      assert {:maybe, _} = Interpreter.match?(pattern1, data5)
      assert :no = Interpreter.match?(pattern1, data6)
      assert {:yes, _} = Interpreter.match?(pattern1, data7)

      pattern2 = quote(do: {1, _, 2})
      data1 = quote(do: {1, 1, 2})
      data2 = quote(do: {1, 2, 3})
      data3 = quote(do: {1, foo()})
      data4 = quote(do: {1, 3, 2})
      data5 = quote(do: {1, 3, foo()})
      data6 = quote(do: {1, 3, []})

      assert {:yes, _} = Interpreter.match?(pattern2, data1)
      assert :no = Interpreter.match?(pattern2, data2)
      assert :no = Interpreter.match?(pattern2, data3)
      assert {:yes, _} = Interpreter.match?(pattern2, data4)
      assert {:maybe, _} = Interpreter.match?(pattern2, data5)
      assert :no = Interpreter.match?(pattern2, data6)
    end

    test "function" do
      pattern = quote(do: [1, _, {1, 2, _}])
      data = quote(do: [1, _, func()])

      assert {:maybe, _} = Interpreter.match?(pattern, data)
    end

    test "deep structure 1" do
      pattern = quote(do: [1, 2, _, 4 | _])
      data = quote(do: [1, 2, %{x: 1, y: 2}, 4, 5, 6])

      assert {:yes, _} = Interpreter.match?(pattern, data)
    end

    test "simple matching clauses" do
      clauses = [
        quote(do: {1, 2}),
        quote(do: {1, 3}),
        quote(do: {1, 4}),
        quote(do: {1, 5}),
        quote(do: {2, 2}),
        quote(do: {_, _})
      ]

      assert {:ok, {2, 2}} = Interpreter.matching_clause({2, 2}, clauses)
    end

    test "simple multimatching clauses" do
      clauses = [
        [:view, quote(do: {_, _})],
        [:update, quote(do: {_, _})],
        [:force_update, quote(do: {_, _, _})]
      ]

      data = [:update, {1, 2}]

      assert {:ok, [:update, _]} = Interpreter.matching_clause(data, clauses)
    end
  end
end
