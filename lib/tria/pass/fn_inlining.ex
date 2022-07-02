defmodule Tria.Pass.FnInlining do

  @moduledoc """
  `fn` functions inlining pass
  Inlines things like `(fn x -> x + 2 end).(1)` to `1 + 2`

  #TODO: error and uncertainty handling
         maybe convert to case when uncertain?
  #TODO: proper context isolation
  #TODO: support `when`
  """

  import Tria.Common
  alias Tria.Interpreter
  alias Tria.Pass.VariablePropagation

  def run(ast) do
    Macro.prewalk(ast, fn code ->
      case run_once(code) do
        {:ok, new_code} -> new_code
        :error -> code
      end
    end)
  end

  def run_once!(ast) do
    case run_once(ast) do
      {:ok, res} -> res
      :error -> raise "This is not fn or it can't be inlined"
    end
  end

  # Now it works only with 
  def run_once({{:".", _, [{:fn, _, clauses}]}, _, args}) do
    clause_to_body =
      Map.new(clauses, fn {:"->", _, [patterns, body]} ->
        {patterns, body}
      end)

    {:ok, matching_patterns} = Interpreter.multimatching_clause(args, Map.keys(clause_to_body))
    matching_body = Map.fetch!(clause_to_body, matching_patterns)

    new_bindings =
      matching_patterns
      |> Enum.zip(args)
      |> Enum.reduce([], fn {pattern, arg}, acc ->
        {:yes, bindings} = Interpreter.match?(pattern, arg)
        acc ++ bindings
      end)
      |> Enum.map(fn {{name, _, context}, value} -> {{name, context}, value} end)
      
    {body, _bindings} = VariablePropagation.run_once(matching_body, new_bindings)
    #inspect_ast(body)
    {:ok, body}
  end
  def run_once(_), do: :error

end
