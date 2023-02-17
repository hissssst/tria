defmodule Tria.Optimizer do

  @moduledoc """
  Main optimizing pipeline
  """

  import Tria.Language, only: [inspect_ast: 2], warn: false
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Optimizer.Depmap
  alias Tria.Optimizer.Pass.{Evaluation, EnumFusion, Peephole}

  @doc """
  Runs optimizer pipeline on Tria AST
  """
  @spec run(Tria.t(), Keyword.t()) :: {Tria.t(), Depmap.t()}
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
    opts = Keyword.put(opts, :return_depmap, true)

    {ast, evaluation_depmap}  = Evaluation.run_while(ast, opts)
    {ast, enum_fusion_depmap} = EnumFusion.run_while(ast, opts)

    depmap = Depmap.merge(evaluation_depmap, enum_fusion_depmap)

    {Peephole.run_while(ast), depmap}
  end

end
