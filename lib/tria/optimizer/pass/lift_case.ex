defmodule Tria.Optimizer.Pass.LiftCase do
  @moduledoc """
  This pass merges case clauses upper

  For example, it transforms this
  ```
  case outer do
    %{x: x} ->
      case x do
        %{y: y} -> y
        %{z: z} -> z
      end

    other ->
      other
  end
  ```

  Into this
  ```
  case outer do
    %{x: %{y: y}} -> y
    %{x: %{z: z}} -> z
    other -> other
  end
  ```
  """

  alias Tria.Optimizer.Pass.Evaluation
  import Tria.Language
  import Tria.Language.Tri

  def run_once({:case, meta, [arg, [do: clauses]]}) do
    {:case, meta, [arg, [do: run_once(clauses)]]}
  end

  def run_once({:fn, meta, clauses}) do
    {:fn, meta, run_once(clauses)}
  end

  def run_once(clauses) when is_list(clauses) do
    Enum.flat_map(clauses, fn
      {:"->", _, [[pattern], {:case, _, [{name, _meta, context} = arg, [do: clauses]]}]} when is_variable(arg) ->
        IO.inspect arg, label: :arg
        pattern_vars = vars_in_pattern(pattern)
        |> IO.inspect(label: :pattern_vars)
        if {name, context} in pattern_vars do
          clauses
          |> isolate_clauses()
          |> Enum.map(fn {:"->", _, [[left], right]} ->
            pattern = Evaluation.run_once!(pattern, matchlist: %{{name, [], context} => left})
            {:"->", [], [[pattern], right]}
          end)
        end

      other ->
        [other]
    end)
  end

  defp vars_in_pattern(pattern) do
    Macro.prewalk(pattern, MapSet.new(), fn
      tri(^_), acc ->
        {nil, acc}

      {name, _, context} = variable, acc when is_variable(variable) ->
        {variable, MapSet.put(acc, {name, context})}

      other, acc ->
        {other, acc}
    end)
    |> elem(1)
  end

  defp isolate_clauses(clauses) do
    # Isolating context the tricky way hehe
    # This can be optimized, but who cares
    {:case, [], [nil, [do: clauses]]} = Evaluation.run_once!({:case, [], [nil, [do: clauses]]})
    clauses
  end

end
