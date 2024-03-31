defmodule Tria.Language.Analyzer do

  @moduledoc """
  Module with functions which perform some code analysis like
  cheking functions purity or safety
  """

  alias Tria.Language.Analyzer.{Purity, Safety}
  alias Tria.Language.Beam

  @doc """
  Checks if ast doesn't produce any side-effects upon evaluation.
  When `true`, this means that there are definitely no side-effects.
  """
  @spec is_pure(Tria.t()) :: boolean()
  def is_pure(ast) do
    Purity.check_analyze(ast)
  end

  @doc """
  Checks if ast can raise.
  When `true`, this means that exception won't definitely be raised
  """
  @spec is_safe(Tria.t()) :: boolean()
  def is_safe(ast) do
    Safety.analyze(ast)
  end

  # Helper function for analyzers
  @doc false
  @spec tria_bodies(Tria.Language.MFArity.t()) :: [Tria.t()] | nil
  def tria_bodies({module, _, _} = mfarity) do
    case Beam.object_code(module) do
      {:ok, object_code} ->
        object_code
        |> Beam.abstract_code!()
        |> Beam.tria_bodies(mfarity)

      :error ->
        nil
    end
  rescue
    error ->
      IO.inspect mfarity, label: :failed
      reraise error, __STACKTRACE__
  end

end
