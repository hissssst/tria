defmodule Tria.Language.Analyzer.PurityTest do
  use ExUnit.Case, async: true

  import Tria.Language.Analyzer.Purity
  import Tria.Language.Tri

  describe "simple" do
    test "pure" do
      tri do
        Kernel.+(1, 2)
      end
      |> check_analyze()
      |> assert()
    end

    test "impure" do
      tri do
        Kernel.self()
      end
      |> check_analyze()
      |> refute()
    end
  end

end
