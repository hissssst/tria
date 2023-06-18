defmodule Tria.Optimizer.Pass.PeepholeTest do

  use ExUnit.Case, async: true

  import Tria.TestHelpers
  alias Tria.Optimizer.Pass.Peephole
  alias Tria.Compiler.AbstractTranslator

  defp run_while(abstract) do
    abstract
    |> AbstractTranslator.to_tria!(as_block: true)
    |> Peephole.run_while()
  end

  describe "Undotting" do
    test "Undotted" do
      abstract [map] do
        map.just_some_map_field
      end
      |> run_while()
      |> assert_tri do
        :erlang.map_get(:just_some_map_field, map)
      end
    end

    test "Not possible" do
      abstract [map] do
        map.self # if map == Kernel, this can be a call
      end
      |> run_while()
      |> assert_tri do
        case _ do
          _
        end
      end
    end
  end

end
