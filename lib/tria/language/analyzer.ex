defmodule Tria.Language.Analyzer do

  import Tria.Language
  alias Tria.Language.Analyzer.Purity

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

end
