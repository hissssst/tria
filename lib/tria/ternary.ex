defmodule Tria.Ternary do
  @moduledoc """
  Ternary logic module with helpers and operations
  """

  defmacro __using__(_opts) do
    quote do
      import Kernel, except: [&&: 2]
      import unquote(__MODULE__)
    end
  end

  defguard is_ternary(x) when x in ~w[yes no maybe]a

  def d && d when is_ternary(d), do: d
  def l && r when :no in [l, r] and is_ternary(l) and is_ternary(r), do: :no
  def l && r when is_ternary(l) and is_ternary(r), do: :maybe
end
