defmodule Tria.Pass.Peephole do

  @moduledoc """
  A list of tiny peephole optimizations implemented as hooks for the Evaluation pass
  """

  alias Tria.Analyzer.Purity
  # alias Tria.Interpreter
  # alias Tria.Pass.Evaluation
  # import Evaluation, only: [put_bind: 3, fetch_bind: 2]
  import Tria.Common
  import Tria.Tri

  def after_hook(code, bindings) do
    case code do
      # Try with literal body
      {:try, _, [[{:do, body} | _clauses]]} = ast ->
        if no_raise_throw?(body), do: body, else: ast

      # Map get with nil default
      tri case(Map.get(map, key), do: clauses) ->
        quoted = quote do: case(Map.get(unquote(map), unquote(key), nil), do: unquote(clauses))
        after_hook(quoted, bindings)

      # Map get with default
      tri(
        case Map.get(map, key, default) do
          default -> default_body
          other -> other_body
        end
      ) = ast ->
        if Macro.quoted_literal?(default) do
          {pattern, variable} =
            case other do
              variable when is_variable(variable) ->
                {variable, variable}

              tri(pattern = variable) when is_variable(variable) ->
                {pattern, variable}

              tri(variable = pattern) when is_variable(variable) ->
                {pattern, variable}

              other ->
                variable = {:variable, [], gen_uniq_context()}
                {quote(do: unquote(other) = unquote(variable)), variable}
            end

          quote do
            case unquote map do
              %{unquote pin key => unquote pattern} when unquote(variable) != unquote(default) ->
                unquote other_body

              %{} ->
                unquote default_body

              term ->
                raise BadMapError, term: term
            end
          end
        else
          ast
        end

      # Default handler
      code ->
        code
    end
  end

  # def before_hook(code, bindings) do
  #   case code do
  #     # With single clause
  #     {:with, meta, [{:"<-", clause_meta, [pattern, arg]}, [do: body, else: else_clauses]]} ->
  #       clauses = [{:"->", clause_meta, [[pattern], body]} | else_clauses]
  #       {:case, meta, [arg, [do: clauses]]}

  #     other ->
  #       other
  #   end
  # end

  defp pin(variable) when is_variable(variable) do
    quote do: ^unquote(variable)
  end
  defp pin(other), do: other

  # TODO think about rewriting this to be more percise
  defp no_raise_throw?(ast) do
    is_fn(ast) or Macro.quoted_literal?(ast) or Purity.check_analyze(ast)
  end
  
end
