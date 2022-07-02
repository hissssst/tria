defmodule Tria.TranslatorTest do
  use ExUnit.Case, async: true

  import Tria.Matcher
  alias Tria.Translator.Elixir, as: ElixirTranslator

  defmodule Example do
    def f(x), do: x * x
  end

  @xxx :"Elixir.Tria.TranslatorTest.X.X.X"
  @xx :"Elixir.Tria.TranslatorTest.X.X"
  @x :"Elixir.Tria.TranslatorTest.X"

  describe "imports" do
    test "simple" do
      assert(
        {:ok, q, _} =
          quote do
            import Example
            f(1)
            f(1, 2, 3)
          end
          |> ElixirTranslator.to_tria(__ENV__)
      )

      assert(
        tri do
          :"Elixir.Tria.TranslatorTest.Example"
          tri({:., _, [:"Elixir.Tria.TranslatorTest.Example", :f]}, _, [1])
          f(1, 2, 3)
        end = q
      )
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

      assert {:ok, quoted, _} = ElixirTranslator.to_tria(quoted, __ENV__)

      assert(
        tri do
          fn ->
            :"Elixir.Tria.TranslatorTest.Example"
            tri({:., _, [:"Elixir.Tria.TranslatorTest.Example", :f]}, _, [1])
          end

          fn ->
            f(2)
          end
        end = quoted
      )
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
      {:ok, q, _} =
        quote do
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

      assert(
        tri do
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
        end = q
      )
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
end
