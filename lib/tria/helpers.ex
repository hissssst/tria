defmodule Tria.Helpers do

  import Tria

  def extract_function(ast) when is_call(ast) do
    {:ok, Macro.decompose_call(ast)}
  end
  def extract_function(_), do: :error

end
