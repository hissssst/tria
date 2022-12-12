defmodule Tria.Optimizer do

  import Tria.Language, only: [inspect_ast: 2], warn: false
  alias Tria.Compiler.SSATranslator
  alias Tria.Optimizer.Pass.Evaluation

  @doc """
  This same as `tria/1` macro, but as a function.
  """
  @spec run(Macro.t(), Keyword.t()) :: Macro.t()
  def run(quoted, opts \\ []) do
    quoted
    |> SSATranslator.from_tria!()
    |> run_while(opts)
  end

  defp run_while(ast, opts) do
    case Evaluation.run_once(ast) do
      {:ok, ast} ->
        run_while(ast, opts)

      {:error, :nothing_to_evaluate} ->
        ast
    end
  end

end
