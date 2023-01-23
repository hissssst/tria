defmodule Tria.Debug do

  @moduledoc """
  Module with helpers for managing debugging state
  """

  @spec flag_debug(boolean()) :: boolean()
  def flag_debug(flag \\ true) do
    was = debugging?()
    :persistent_term.put(:tria_debugging, flag)
    was
  end

  @spec debugging?() :: boolean()
  def debugging? do
    :persistent_term.get(:tria_debugging, false)
  end

end
