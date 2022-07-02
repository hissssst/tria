defmodule Tria.Pass.ConstantEvaluation do
  @moduledoc """
  This pass evaluates the passed functions at compile time
  """

  import Tria.Common

  def run(code, functions) do
    Macro.prewalk(code, fn
      dot_call(module, function, args) = call ->
        case run_once(module, function, args, functions) do
          {:ok, res} ->
            Macro.escape(res)

          :error ->
            call
        end

      other ->
        other
    end)
  end

  # def run_once(function, args, functions) do
  # end

  def run_once(module, function, args, functions) do
    arity = length(args)
    if {module, function, arity} in functions do
      Enum.reduce_while(args, [], fn arg, acc ->
        if Macro.quoted_literal?(arg), do: {:cont, [arg | acc]}, else: {:halt, nil}
      end)
      |> case do
        nil ->
          :error

        list ->
          args =
            list
            |> Enum.reverse()
            |> Enum.map(fn arg ->
              {res, []} = Code.eval_quoted(arg)
              res
            end)

          {:ok, apply(module, function, args)}
      end
    end
  end
end
