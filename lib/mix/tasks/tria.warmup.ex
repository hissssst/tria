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

  During analysis a question may pop up, for example:
  ```
  :cover.analyse(:coverage, :line)

  Is pure? [y(yes); n(no); S(show); s(stack)]
  ```

  In this case, you need to respond whether `:cover.analyze/2` is a pure function. You don't
  need to take a purity of expressions in arguments into account, they're there just to provide
  context.

  This is required since some functions in OTP are bifs or nifs, and they can't be analyzed
  for purity by Tria.

  ### Options:

  * `--loaded` -- to warm up cache only from modules which are loaded by default
  * `--available` -- to warm up cache from all modules in code path
  """

  @shortdoc "Warms up tria cache files"

  import Tria.Compiler.ElixirTranslator, only: [unalias: 1]
  alias Tria.Language.Beam
  alias Tria.Language.Analyzer.Purity

  def run(args) when is_list(args) do
    {functions, modules} = run parse_args args

    IO.write [
      IO.ANSI.clear_line(),
      IO.ANSI.green(),
      "\nWarmup completed",
      IO.ANSI.reset(),
      "\nChecked #{functions} functions in #{modules - 1} modules\n"
    ]
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

    Enum.reduce(modules, {0, 1}, fn module, {all_functions, index} ->
      with(
        {:ok, object_code} <- Beam.object_code(module),
        {:ok, abstract_code} <- Beam.abstract_code(object_code)
      ) do
        functions = Beam.functions(abstract_code)
        flength = length(functions)

        Enum.reduce(functions, {1, now()}, fn {function, arity}, {findex, ts} ->
          status(length, index, module, flength, findex)
          Purity.check_analyze_mfarity({module, function, arity})
          now = now()
          difference = now - ts
          if difference > 1000 do
            IO.write [
              IO.ANSI.clear_line(),
              "\r#{inspect module}.#{function}/#{arity} took #{difference}ms to analyze!\n"
            ]
          end

          {findex + 1, now}
        end)
      end
      |> case do
        {functions, _} ->
          {all_functions + functions, index + 1}

        _ ->
          {all_functions, index + 1}
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

  defp now do
    :erlang.monotonic_time(:millisecond)
  end

end
