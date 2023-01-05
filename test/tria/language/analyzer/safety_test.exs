defmodule Tria.Language.Analyzer.SafetyTest do
  use ExUnit.Case, async: true

  import Tria.Language.Analyzer.Safety
  import Tria.Language.Tri

  describe "simple" do
    test "safe" do
      tri do
        self()
      end
      |> analyze()
      |> assert()
    end

    test "unsafe division" do
      tri do
        :erlang./(1, 0)
      end
      |> analyze()
      |> refute()
    end

    test "unsafe throw" do
      tri do
        throw :fuck
      end
      |> analyze()
      |> refute()
    end

    test "unsafe raise" do
      tri do
        raise "Oopsie"
      end
      |> analyze()
      |> refute()
    end

    test "unsafe math" do
      tri do
        :math.sqrt x
      end
      |> analyze()
      |> refute()
    end

    test "unsafe case" do
      tri do
        case x do
          true -> throw :oops
          _ -> nil
        end
      end
      |> analyze()
      |> refute()
    end
  end

  describe "regression" do
    test "well" do
      tri do
        case (case highlight_84675 do
               underscore_84771
               when :erlang.orelse(
                      Kernel.===(underscore_84771, false),
                      Kernel.===(underscore_84771, nil)
                    ) ->
                 nil

               _ ->
                 <<45, 104, 105, 103, 104, 108, 105, 103, 104, 116>>
             end) do
         underscore_84835 when :erlang.is_binary(underscore_84835) ->
           underscore_84835

         underscore_84867 ->
           String.Chars.to_string(
             case highlight_84675 do
               underscore_84771
               when :erlang.orelse(
                      Kernel.===(underscore_84771, false),
                      Kernel.===(underscore_84771, nil)
                    ) ->
                 nil

               _ ->
                 <<45, 104, 105, 103, 104, 108, 105, 103, 104, 116>>
             end
           )
        end
      end
      |> analyze()
      |> refute()
    end
  end
end
