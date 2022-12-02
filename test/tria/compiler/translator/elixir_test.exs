defmodule Tria.Compiler.ElixirTranslatorTest do
  use ExUnit.Case, async: true

  import Tria.Language, only: [inspect_ast: 2], warn: false
  import Tria.Language.Tri
  import Tria.TestHelpers
  alias Tria.Compiler.ElixirTranslator

  defmodule Example do
    def f(x), do: x * x
  end

  # This options make `tri` behave almost like a `quote`, but without meta
  @tri_opts to_tria: false, meta: false

  @xxx :"Elixir.Tria.Compiler.ElixirTranslatorTest.X.X.X"
  @xx  :"Elixir.Tria.Compiler.ElixirTranslatorTest.X.X"
  @x   :"Elixir.Tria.Compiler.ElixirTranslatorTest.X"

  describe "Imports" do
    test "simple" do
      assert(
        {:ok, translated, _} =
          quote do
            import Example
            f(1)
            f(1, 2, 3)
          end
          |> ElixirTranslator.to_tria(__ENV__)
      )

      assert_tri translated do
          :"Elixir.Tria.Compiler.ElixirTranslatorTest.Example"
          tri({:., _, [:"Elixir.Tria.Compiler.ElixirTranslatorTest.Example", :f]}, _, [1])
          f(1, 2, 3)
      end
    end

    test "contexted" do
      quoted =
        quote do
          fn ->
            import Example
            f(1)
          end

          fn ->
            f(2)
          end
        end

      assert {:ok, translated, _} = ElixirTranslator.to_tria(quoted, __ENV__)

      assert_tri translated do
        fn ->
          :"Elixir.Tria.Compiler.ElixirTranslatorTest.Example"
          tri({:., _, [:"Elixir.Tria.Compiler.ElixirTranslatorTest.Example", :f]}, _, [1])
        end

        fn ->
          f(2)
        end
      end
    end
  end

  defmodule X.X do
    def f(x), do: x + x

    defmodule X do
      def f(x), do: x * x
    end
  end

  defmacrop fdot1(module) do
    quote do
      {{:., _, [unquote(module), :f]}, _, [1]}
    end
  end

  describe "flow" do
    test "case" do
      {:ok, translated, _} =
        tri do
          case (
                 alias X.X
                 X.f(1)
               ) do
            1 ->
              alias X.X
              X.f(1)

            2 ->
              X.f(1)
          end
        end
        |> ElixirTranslator.to_tria(__ENV__)

      assert_tri translated do
        case (
               tri(@xx)
               tri(fdot1(@xx))
             ) do
          1 ->
            tri(@xxx)
            tri(fdot1(@xxx))

          2 ->
            tri(fdot1(@xx))
        end
      end
    end

    test "with" do
      {:ok, q, _} =
        quote do
          with(
            _ <-
              (
                alias X.X
                X.f(1)
              ),
            _ <-
              (
                alias X.X
                X.f(1)
              )
          ) do
            X.f(1)
          else
            _ -> X.f(1)
          end
        end
        |> ElixirTranslator.to_tria(__ENV__)

      assert(
        tri do
          with(
            _ <-
              (
                tri(@xx)
                tri(fdot1(@xx))
              ),
            _ <-
              (
                tri(@xxx)
                tri(fdot1(@xxx))
              )
          ) do
            tri(fdot1(@xxx))
          else
            _ -> tri(fdot1(@x))
          end
        end = q
      )
    end

    test "cond" do
      {:ok, q, _} =
        quote do
          alias X.X

          cond do
            1 ->
              alias X.X
              X.f(1)

            2 ->
              X.f(1)
          end
        end
        |> ElixirTranslator.to_tria(__ENV__)

      assert(
        tri do
          tri(@xx)

          cond do
            1 ->
              tri(@xxx)
              tri(fdot1(@xxx))

            2 ->
              tri(fdot1(@xx))
          end
        end = q
      )
    end
  end

  test "Ultimate" do
    {:ok, q, _} =
      quote do
        fn ->
          # Simple in structure inheritance
          [
            (
              alias X.X
              X.f(1)
            ),
            X.f(1),
            X.f(1)
          ]

          X.f(1)
        end

        fn ->
          # Clause non-inheritance
          alias X.X

          case 1 do
            X ->
              X.f(1)

            X ->
              alias X.X
              X.f(1)
          end
        end

        # Outer to inner inheritance
        require X.X, as: X

        fn ->
          # alias-import composition
          import X.X
          f(1)
          f(1, 2, 3)
        end
      end
      |> ElixirTranslator.to_tria(__ENV__)

    assert(
      tri do
        fn ->
          [
            (
              tri(@xx)
              tri(fdot1(@xx))
            ),
            tri(fdot1(@xx)),
            tri(fdot1(@xx))
          ]

          tri(fdot1(@xx))
        end

        fn ->
          tri(@xx)

          case 1 do
            tri(@xx) ->
              tri(fdot1(@xx))

            tri(@xx) ->
              tri(@xxx)
              tri(fdot1(@xxx))
          end
        end

        tri(@xx)

        fn ->
          tri(@xxx)
          tri(fdot1(@xxx))
          f(1, 2, 3)
        end
      end = q
    )
  end

  describe "Meta preservation" do
    defp fetch({_, meta, _}, key), do: Keyword.fetch(meta, key)

    test "Line" do
      quoted =
        quote location: :keep do
          x + y
          x = z
          x = 1
        end

      {:ok, {file, line}} = fetch(quoted, :keep)
      new_quoted = ElixirTranslator.to_tria!(quoted, __ENV__)
      assert {:ok, {file, line}} == fetch(new_quoted, :keep)
      assert {:ok, file} == fetch(new_quoted, :file)
      assert {:ok, line} == fetch(new_quoted, :line)
    end
  end

  describe "Captures" do
    test "just works" do
      translated =
        tri do
          Enum.map(list, & &1 + 1)
        end
        |> ElixirTranslator.to_tria!(__ENV__)

      assert_tri translated do
        Enum.map(_list, fn x -> Kernel.+(x, 1) end)
      end
    end

    test "& capture multiple args" do
      quoted =
        quote do
          fn combination ->
            Enum.reduce(combination, 1, &(length(&1) * &2))
          end
        end
        |> ElixirTranslator.to_tria!(__ENV__)

      assert_tri quoted do
        fn combination ->
          Enum.reduce(combination, 1, fn first, second ->
            Kernel.*(Kernel.length(first), second)
          end)
        end
      end

      assert 3 == length Enum.uniq [combination, first, second]
    end
  end

  describe "try rescue" do
    test "in" do
      translated =
        quote do
          try do
            raising
          rescue
            error in ArgumentError -> error
          end
        end
        |> ElixirTranslator.to_tria!(__ENV__)

      # # I could write this, but `assert` is buggy as hell
      # assert_tri translated, debug: true do
      #   try do
      #     raising
      #   rescue
      #     error in ArgumentError -> error
      #   end
      # end

      assert {:try, _,
       [
         [
           do: raising,
           rescue: [{:->, _, [[{:in, _, [error, {:__aliases__, _, [:ArgumentError]}]}], error]}]
         ]
       ]} = translated

      assert raising != error
    end

    test "else" do
      quote do
        try do
          raising
        else
          1 -> 2
        end
      end
      |> ElixirTranslator.to_tria!(__ENV__)
      |> assert_tri do
        try do
          _raising
        else
          1 -> 2
        end
      end
    end

    test "full-blown" do
      # Without recuse because waiting for 1.16
      quote do
        x = 1
        try do
          x = 2
          raising
        else
          2 -> x
          x -> x
        catch
          3 -> x
          x -> x
        # rescue
        #   x in Error -> x
        after
          x
        end
      end
      |> ElixirTranslator.to_tria!(__ENV__)
      |> assert_tri do
        x = 1
        try do
          x = 2
          _raising
        else
          2 -> x
          x -> x
        catch
          3 -> x
          x -> x
        # rescue
        #   x in Error -> x
        after
          x
        end
      end
    end
  end

  describe "__ENV__" do
    test "__ENV__" do
      quote do
        __ENV__
      end
      |> ElixirTranslator.to_tria!(__ENV__)
      |> assert_tri do
        # TODO think of better assertion
        %{tri_splicing _}
      end
    end
  end

end
