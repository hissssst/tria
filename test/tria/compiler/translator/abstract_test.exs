defmodule Tria.Compiler.AbstractTranslatorTest do

  use ExUnit.Case, async: true

  alias Tria.Compiler.AbstractTranslator
  import Tria.Language, only: [is_context: 1, is_pinned: 1, inspect_ast: 2], warn: false
  import Tria.Language.Tri, warn: false
  import Tria.Language.Meta
  import Tria.TestHelpers

  defp to_tria!(ast) do
    AbstractTranslator.to_tria!(ast, env: __ENV__, as_block: true)
  end

  describe "Maps" do
    test "map_cons" do
      abstract do
        x = %{key: :value, other_key: :other_value}
        %{x | key: :other_value, other_key: :value}
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        x = %{key: :value, other_key: :other_value}
        %{x | key: :other_value, other_key: :value}
      end
    end
  end

  describe "`rescue` retranslation" do
    test "x in Error" do
      abstract do
        try do
          1
        rescue
          x in ArgumentError -> x
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          tri(:in, _, [underscore, ArgumentError]) ->
            x = underscore
            x
        end
      end
    end

    test "Error" do
      abstract do
        try do
          1
        rescue
          ArgumentError -> 2
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          tri(:in, _, [_underscore, ArgumentError]) -> 2
        end
      end
    end

    test "Throw" do
      abstract do
        try do
          1
        catch
          x -> x
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        catch
          x ->x
        end
      end
    end

    test "Non-erlang Error" do
      abstract do
        try do
          1
        rescue
          URI.Error -> 2
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          tri(:in, _, [_underscore, URI.Error])-> 2
        end
      end
    end

    test "No errors" do
      abstract do
        try do
          1
        rescue
          _ -> 2
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          tri(name, meta, context) -> 2
        end
      end

      assert is_atom name
      assert is_list meta
      assert is_context context
    end

    test "__STACKTRACE__" do
      abstract do
        try do
          1
        rescue
          _ -> IO.inspect __STACKTRACE__
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          _ -> IO.inspect tri(:__STACKTRACE__, meta, context)
        end
      end

      assert is_list meta
      assert is_context context
    end

    test "Multiple errors" do
      abstract do
        try do
          1
        rescue
          e in [ArithmeticError, ArgumentError, URI.Error] -> e
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        rescue
          underscore in [ArithmeticError, ArgumentError, URI.Error] ->
            e = underscore
            e
        end
      end
    end

    test "Kinds" do
      abstract do
        try do
          1
        catch
          :exit, reason ->
            {:exited_with, reason}

          kind, reason ->
            {kind, reason}
        end
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        try do
          1
        catch
          :exit, reason ->
            {:exited_with, reason}

          kind, reason2 ->
            {kind, reason2}
        end
      end
    end

    test "Else" do
      abstract do
        try do
          1
        catch
          x -> x
        else
          1 -> 2
          3 -> 4
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        catch
          x -> x
        else
          1 -> 2
          3 -> 4
        end
      end
    end

    test "After" do
      abstract do
        try do
          1
        after
          2
        end
      end
      |> to_tria!()
      |> assert_tri do
        try do
          1
        after
          2
        end
      end
    end
  end

  describe "Pins" do
    test "Simple" do
      abstract do
        x = 0
        [^x, ^x] = [0, 0]
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        x = 0
        [^x, ^x] = [0, 0]
      end
    end

    test "Reset" do
      abstract do
        x = 0
        x = 1
        ^x = 1
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        x1 = 0
        x2 = 1
        ^x2 = 1
      end
    end

    test "Reused" do
      abstract do
        x = 0
        ^x = 1
        y = [x]
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        x = 0
        ^x = 1
        y = [x]
      end
    end

    test "Different clauses" do
      abstract do
        case :first_clause do
          :first_clause ->
            x = 0
            ^x = 0

          :second_clause ->
            x = 2
        end
      end
      |> to_tria!()
      |> unmeta()
      |> assert_tri do
        case :first_clause do
          :first_clause ->
            x2 = 0
            ^x2 = 0

          :second_clause ->
            x3 = 2
        end
      end
    end

    test "Funs not pinned" do
      alias Tria.Language.Codebase

      [{_, binary}] =
        quote do
          defmodule X do
            def f(x, f \\ &to_string/1) do
              f.(x)
            end
          end
        end
        |> Code.compile_quoted("nofile")

      binary
      |> Codebase.fetch_abstract_code()
      |> elem(1)
      |> Enum.find_value(fn
        {:function, _, :f, 1, [{:clause, _, _, _, body}]} -> body
        _ -> false
      end)
      |> to_tria!()
      |> assert_tri do
        f(x, fn y -> _ end)
      end

      assert not is_pinned y
      assert x != y
    end
  end

  describe "Guards" do
    test "Elixir" do
      abstract do
        case 1 do
          _ when 1 > 2 and 3 + 4 < length([1, 2, 3 | 4]) or tuple_size({1, 2, 3}) > 1 ->
            :ok
        end
      end
      |> to_tria!()
      |> assert_tri do
        case 1 do
          _
          when :erlang.orelse(
                 :erlang.andalso(1 > 2, 3 + 4 < :erlang.length([1, 2, 3 | 4])),
                 :erlang.tuple_size({1, 2, 3}) > 1
               ) ->
            :ok
        end
      end
    end
  end
end
