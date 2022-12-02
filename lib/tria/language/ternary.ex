defmodule Tria.Language.Ternary do
  @moduledoc """
  Ternary logic module with helpers and operations
  """

  @type t :: :yes | :maybe | :no
  @type t(something) :: {:yes | :maybe, something} | :no

  defmacro __using__(_opts) do
    imports =
      __CALLER__.functions
      |> Keyword.get(Kernel, [])
      |> Kernel.++(Keyword.get(__CALLER__.macros, Kernel, []))
      |> Keyword.delete(:"&&")

    quote do
      import Kernel, only: unquote(imports)
      import unquote(__MODULE__)
    end
  end

  defguard is_ternary(x) when x in ~w[yes no maybe]a

  def d && d when is_ternary(d), do: d
  def l && r when :no in [l, r] and is_ternary(l) and is_ternary(r), do: :no
  def l && r when is_ternary(l) and is_ternary(r), do: :maybe

  def greater?(:yes, _), do: true
  def greater?(:maybe, :no), do: true
  def greater?(_, _), do: false

end
