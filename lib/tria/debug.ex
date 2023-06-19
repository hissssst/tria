defmodule Tria.Debug do

  @moduledoc """
  Module with helpers for debugging
  and managing debugging global state
  """

  import Kernel, except: [inspect: 1, inspect: 2]

  @type tag :: atom()

  @spec flag_debug([atom()] | :all) :: :ok
  def flag_debug(tags \\ :all) do
    :persistent_term.put(:tria_debugging, tags)
    :ok
  end

  @spec debugging?(tag) :: boolean()
  def debugging?(nil) do
    :all == :persistent_term.get(:tria_debugging, [])
  end
  def debugging?(tag) do
    case :persistent_term.get(:tria_debugging, []) do
      :all -> true
      tags -> tag in tags
    end
  end

  @spec inspect_ast(ast, Keyword.t()) :: ast
        when ast: Tria.t() | Macro.t()
  def inspect_ast(ast, opts \\ []) do
    if debugging?(opts[:label]) do
      Tria.Language.inspect_ast(ast, opts)
    else
      ast
    end
  end

  @spec inspect(value, Keyword.t()) :: value
                when value: any()
  def inspect(value, opts \\ []) do
    if debugging?(opts[:label]) do
      IO.inspect(value, opts)
    else
      value
    end
  end

  @spec puts(String.t()) :: :ok
  def puts(string) do
    if debugging?(nil), do: IO.puts(string)
    :ok
  end

end
