defmodule Tria.Optimizer.Pass.Enuming do
  @moduledoc """
  Enuming
  """

  @common_ops ~w[filter reject map]a

  import Tria.Language
  import Tria.Language.Tri
  alias Tria.Optimizer.Evaluation2

  def run_once!(ast, _opts \\ []) do
    {argument, steps} = unchain(ast)

    steps =
      steps
      |> unreject()
      |> filter_join()
  end

  def unreject([{enum_or_stream, :reject, [the_fn]} | tail]) do
    body =
      tri do
        fn x -> ! the_fn.(x) end
      end

    [{enum_or_stream, :filter, [body]} | unreject(tail)]
  end

  def filter_join(_) do

  end

  defp unchain(chain, acc \\ [])

  defp unchain(dot_call(enum_or_stream, function, args), acc) when enum_or_stream in [Enum, Stream] do
    case {function, args} do
      {common_op, [subj | args]} when common_op in @common_ops ->
        unchain(subj, [{enum_or_stream, common_op, args} | acc])

      _ ->
        raise "Not implemented"
    end
  end

  defp unchain(other, acc), do: {other, acc}

end
