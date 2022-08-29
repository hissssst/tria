defmodule Tria.Pass.FnInliningTest do
  use ExUnit.Case

  import Tria.Common
  import Tria.Tri
  alias Tria.Pass.FnInlining

  test "simple" do
    res =
      quote do
        x = 1
        (fn y -> {:ok, y} end).(x)
      end
      |> Tria.Translator.Elixir.to_tria(__ENV__)
      |> elem(1)
      |> FnInlining.run()
      |> inspect_ast()

    assert(tri do
      x = 1
      {:ok, x}
    end = res)
  end

  # @tag skip: true
  test "pathex" do
    res =
      quote do
        (fn
           :view, {structure, func} ->
             case structure do
               %{x: %{x: %{x: x}}} -> func.(x)
               _ -> :error
             end

           :update, _ ->
             :here_goes_update
         end).(:view, {input, fn x -> {:ok, x} end})
      end
      |> Tria.Translator.Elixir.to_tria(__ENV__)
      |> elem(1)
      |> FnInlining.run()
      |> inspect_ast()

    assert(tri do
      case input do
        %{x: %{x: %{x: x}}} -> {:ok, x}
        _ -> :error
      end
    end = res)
  end
end
