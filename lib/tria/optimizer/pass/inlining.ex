defmodule Tria.Optimizer.Pass.Inlining do

  @moduledoc """
  Pass for functions inlining
  """

  use Tria.Optimizer.Pass
  import Tria.Language
  import Tria.Language.Tri

  alias Tria.Language.FunctionRepo
  alias Tria.Optimizer.Pass.Evaluation

  def run_once(ast, _opts) do
    state = %{
      size: size_ast(ast)
    }

    prewalk(ast, state, fn
      dot_call(module, function, args) = call, state ->
        case inline(module, function, args, state) do
          {:yes, inlined} ->
            {inlined, state}

          _ ->
            {call, state}
        end

      other, state ->
        {other, state}
    end)

    {:ok, ast}
  end

  def inline(module, function, args, _state) do
    arity = length(args)
    mfa = {module, function, arity}
    case FunctionRepo.lookup(mfa, :tria) do
      nil ->
        :no

      # Simple case for functions with defaults
      {:fn, _, [{:"->", _, [function_args, dot_call(^module, ^function, more_args)]}]} = the_fn ->
        diff = more_args -- function_args
        if Enum.all?(diff, &vared_literal?/1) do
          inlined =
            tri to_ssa: true do
              the_fn.(args)
            end
            |> Evaluation.run_once!()

          {:yes, inlined}
        else
          :no
        end

      _ ->
        :no
    end
  end

end
