defmodule Tria.CompilerTest do

  use ExUnit.Case, async: true

  alias Tria.Compiler
  alias Tria.Compiler.ContextServer

  describe "Recompilation from BEAM works" do
    setup do
      context = :"test_context_#{:erlang.unique_integer([:positive])}"
      ContextServer.start(context)
      {:ok, context: context}
    end

    test "Module just gets recompiled", %{context: context} do
      {:module, module, beam, _definitions} =
        defmodule A do
          def f(x) do
            x + 1
          end
        end

      Compiler.recompile_from_beam(module, beam, %{context: context, file: __ENV__.file})
      assert [{_, _}] = ContextServer.generate(context)
    end

    test "Binary recompilation works fine", %{context: context} do
      {:module, module, beam, _definitions} =
        defmodule B do
          def size(x), do: byte_size(x)

          def f(string, l) do
            <<x :: size(l), _ :: binary>> = string
            x
          end
        end

      Compiler.recompile_from_beam(module, beam, %{context: context, file: __ENV__.file})
      assert [{_, _}] = ContextServer.generate(context)
    end
  end

end
