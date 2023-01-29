defmodule Tria.Language.Analyzer do

  @moduledoc """
  Module with functions which perform some code analysis like
  cheking functions purity or safety
  """

  import Tria.Language
  alias Tria.Language.Analyzer.Purity
  alias Tria.Language.Analyzer.Safety

  @doc """
  This function should be used only for debugging purposes
  """
  def info(ast, _opts \\ []) do
    cond do
      quoted_literal?(ast) ->
        :literal

      vared_literal?(ast) ->
        :vared

      Purity.check_analyze(ast) ->
        :pure

      true ->
        :impure
    end
  end

  def is_pure(ast) do
    Purity.check_analyze(ast)
  end

  def is_safe(ast) do
    Safety.analyze(ast)
  end

end
