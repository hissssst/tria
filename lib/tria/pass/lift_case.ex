defmodule Tria.Pass.LiftCase do
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

  alias Tria.Pass.VariablePropagation
  import Tria.Tri
  import Tria.Common

  def test do
    {:ok, quoted, _} =
      quote do
        case outer do
          %{x: x} ->
            case x do
              %{y: y} -> y
              %{z: z} -> z
            end

          other ->
            other
        end
      end
      |> Tria.Translator.Elixir.to_tria(__ENV__)

    run_once(quoted)
    |> inspect_ast()
  end

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
            {pattern, _} = VariablePropagation.run_once(pattern, %{{name, context} => left})
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

  defp vars_literals(ast, vars \\ [])
  defp vars_literals({name, _, context} = v, vars) when is_variable(v) do
    {true, [{name, context} | vars]}
  end
  defp vars_literals([head | tail], vars) do
    case vars_literals(head, vars) do
      {true, vars} ->
        vars_literals(tail, vars)

      false ->
        false
    end
  end
  defp vars_literals({tm, _meta, args}, vars) when tm in ~w[{} %{}]a do
    vars_literals(args, vars)
  end
  defp vars_literals({left, right}, vars) do
    case vars_literals(left, vars) do
      {true, vars} ->
        vars_literals(right, vars)

      false ->
        false
    end
  end
  defp vars_literals(other, vars) do
    if Macro.quoted_literal?(other) do
      {true, vars}
    else
      false
    end
  end

  defp isolate_clauses(clauses) do
    # Isolating context the tricky way hehe
    # This can be optimized, but who cares
    {{:case, [], [nil, [do: clauses]]}, _} = VariablePropagation.run_once({:case, [], [nil, [do: clauses]]})
    clauses
  end

end
