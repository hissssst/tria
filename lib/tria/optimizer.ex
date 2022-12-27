defmodule Tria.Optimizer do

  import Tria.Language, only: [inspect_ast: 2], warn: false
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Optimizer.Pass.Evaluation
  alias Tria.Optimizer.Pass.EnumFusion

  @doc """
  This same as `tria/1` macro, but as a function.
  """
  @spec run(Macro.t(), Keyword.t()) :: Macro.t()
  def run(quoted, opts \\ []) do
    quoted
    |> SSATranslator.from_tria!()
    |> Tracer.tag_ast(label: :optimizer_after_ssa)
    |> run_while(opts)
  rescue
    e ->
      inspect_ast(quoted, label: :failed_run_for)
      reraise e, __STACKTRACE__
  end

  defp run_while(ast, opts) do
    ast
    |> Evaluation.run_while(opts)
    # |> EnumFusion.run_while()
    # |> Evaluation.run_while()
  end

end
