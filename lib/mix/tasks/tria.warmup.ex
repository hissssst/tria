defmodule Mix.Tasks.Tria.Warmup do
  use Mix.Task

  @moduledoc """

  """

  @shortdoc "Checks all loaded modules or modules path"

  import Tria.Compiler.ElixirTranslator, only: [unalias: 1]
  alias Tria.Language.Codebase
  alias Tria.Language.Analyzer.Purity

  def run(args) when is_list(args) do
    run parse_args args
  end

  def run(%{from: :loaded}) do
    for {module, _} <- :code.all_loaded(), {function, arity} <- Codebase.fetch_functions(module) do
      Purity.check_analyze_mfarity({module, function, arity})
    end
  end

  def run(%{from: :available} = opts) do
    run(%{opts | from: :loaded})
    for {module, _file, false} <- :code.all_available() do
      module = :"#{module}"
      # {:module, ^module} = :code.load_file(module)
      for {function, arity} <- Codebase.fetch_functions(module) do
        Purity.check_analyze_mfarity({module, function, arity})
      end
    end
  end

  defp parse_args(args, opts \\ %{from: :loaded, set: nil})
  defp parse_args(["--loaded" | tail], opts) do
    parse_args(tail, %{opts | from: :loaded})
  end
  defp parse_args(["--available" | tail], opts) do
    parse_args(tail, %{opts | from: :available})
  end
  defp parse_args(["--set", "true" | tail], opts) do
    parse_args(tail, %{opts | set: true})
  end
  defp parse_args(["--set", "false" | tail], opts) do
    parse_args(tail, %{opts | set: false})
  end
  defp parse_args(["--only", mfarity | tail], opts) do
    {:/, _, [{:., _, [aliased, function]}, arity]} = Code.string_to_quoted!(mfarity)
    parse_args(tail, %{opts | from: {unalias(aliased), function, arity}})
  end
  defp parse_args([], opts), do: opts

end
