defmodule Mix.Tasks.Tria.Warmup do
  use Mix.Task

  @moduledoc """
  # Cache warmup

  Tria uses cache ets files located in `priv` directory to
  optimize evaluation of some function traits like purity
  or safety. This task warms up the caches.

  If you want to regenerate cache file, you can just
  delete it and then call this task.

  ### Example:

  Basically you may want to run something like this once

  ```sh
  mix tria.warmup --available
  ```

  ### Options:

  * `--loaded` -- to warm up cache only from modules which are loaded by default
  * `--available` -- to warm up cache from all modules in code path
  """

  @shortdoc "Warms up tria cache files"

  import Tria.Compiler.ElixirTranslator, only: [unalias: 1]
  alias Tria.Language.Beam
  alias Tria.Language.Analyzer.Purity

  def run(args) when is_list(args) do
    run parse_args args
  end

  def run(%{from: :loaded}) do
    :code.all_loaded()
    |> Enum.map(fn {module, _filename} -> module end)
    |> check()
  end

  def run(%{from: :available}) do
    :code.all_available()
    |> Enum.map(fn {module, _, _} -> :"#{module}" end)
    |> check()
  end

  def check(modules) do
    length = length modules
    modules
    |> Stream.with_index(1)
    |> Enum.each(fn {module, index} ->
      with(
        {:ok, object_code} <- Beam.object_code(module),
        {:ok, abstract_code} <- Beam.abstract_code(object_code)
      ) do
        functions = Beam.functions(abstract_code)
        flength = length(functions)

        functions
        |> Stream.with_index(1)
        |> Enum.each(fn {{function, arity}, findex} ->
          status(length, index, module, flength, findex)
          Purity.check_analyze_mfarity({module, function, arity})
        end)
      end
    end)
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

  defp status(all_modules, current_module, module, all_functions, current_function) do
    IO.write [
      IO.ANSI.clear_line(),
      "\r#{current_module}/#{all_modules} #{inspect module} #{current_function}/#{all_functions}"
    ]
  end

end
