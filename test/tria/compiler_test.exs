defmodule Tria.CompilerTest do

  use ExUnit.Case, async: true

  alias Tria.Compiler
  alias Tria.Compiler.ContextServer

  setup do
    context = :"test_context_#{:erlang.unique_integer([:positive])}"
    ContextServer.start(context)
    {:ok, context: context}
  end

  describe "Recompilation from BEAM" do
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

  describe "Annotations" do
    test "Annotation is represented in FunctionRepo", %{context: context} do
      alias Tria.Language.FunctionRepo

      {:module, module, beam, _definitions} =
        defmodule C do
          use Tria

          @tria safe: true, optimize: false
          def f(x), do: x + 1

          @tria safe: false
          def g(x), do: x
        end

      Compiler.recompile_from_beam(module, beam, %{context: context, file: __ENV__.file})

      assert true  == FunctionRepo.lookup({C, :f, 1}, :safe_cache)
      assert false == FunctionRepo.lookup({C, :f, 1}, :optimize)
      assert false == FunctionRepo.lookup({C, :g, 1}, :safe_cache)
    end

    test "Wrong annotations raises", %{context: context} do
      assert_raise CompileError, fn ->
        defmodule D do
          use Tria

          @tria this_option_does_not_exist: false
          def f(x), do: x + 1
        end
      end

      assert_raise CompileError, fn ->
        defmodule E do
          use Tria

          @tria safe: :yes
          def f(x), do: x + 1
        end
      end
    end
  end

end
