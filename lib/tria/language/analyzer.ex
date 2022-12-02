defmodule Tria.Language.Analyzer do

  import Tria.Language
  alias Tria.Language.Analyzer.Purity

  def info(ast, _opts \\ []) do
    cond do
      Macro.quoted_literal?(ast) ->
        :literal

      vared_literal?(ast) ->
        :vared

      Purity.check_analyze(ast) ->
        :pure

      true ->
        :impure
    end
  end

  def vared_literal?(ast) do
    case ast do
      [head | tail] ->
        vared_literal?(head) and vared_literal?(tail)

      {left, right} ->
        vared_literal?(left) and vared_literal?(right)

      vl when is_variable(vl) or is_literal(vl) ->
        true

      {s, _, children} when s in ~w[%{} {} |]a ->
        vared_literal?(children)

      _ ->
        false
    end
  end


end
