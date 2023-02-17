defmodule Tria.Debug do

  @moduledoc """
  Module with helpers for debugging
  and managing debugging global state
  """

  import Kernel, except: [inspect: 1, inspect: 2]

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

  @spec inspect_ast(ast, Keyword.t()) :: ast
        when ast: Tria.t() | Macro.t()
  def inspect_ast(ast, opts \\ []) do
    if debugging?() do
      Tria.Language.inspect_ast(ast, opts)
    else
      ast
    end
  end

  @spec inspect(value, Keyword.t()) :: value
                when value: any()
  def inspect(value, opts \\ []) do
    if debugging?() do
      IO.inspect(value, opts)
    else
      value
    end
  end

  @spec puts(String.t()) :: :ok
  def puts(string) do
    if debugging?(), do: IO.puts(string)
    :ok
  end

end
