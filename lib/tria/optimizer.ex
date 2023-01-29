defmodule Tria.Optimizer do

  @moduledoc """
  Main optimizing pipeline
  """

  import Tria.Language, only: [inspect_ast: 2], warn: false
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Optimizer.Pass.{Evaluation, EnumFusion, Peephole}

  @doc """
  Runs optimizer pipeline on Tria AST
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
    # |> EnumFusion.run_while(opts)
    |> Peephole.run_while()
  end

end
