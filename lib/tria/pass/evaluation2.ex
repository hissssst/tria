defmodule Tria.Pass.Evaluation2 do

  @moduledoc """
  This pass tries to evaluate everything what
  can be preevaluated
  """

  alias Tria.Interpreter
  import Tria.Common

  defstruct [
    bindings: %{}
  ]

  ## Public API

  def run_once!(ssa_ast, opts \\ []) do
    run(ssa_ast, %__MODULE__{})
  end

  ## Runners

  def run(ast, state) do
    case ast do
      {:"=", meta, [left, right]} ->
        {left, left_state} = run_match(left, state)
        {right, right_state} = run(right, state)
        block =
          case Interpreter.match(left, right) do
            {_yes_or_maybe, binds} ->
              {:__block__, [],
                Enum.map(binds, fn {left, right} -> {:=, meta, [left, right]} end)
                ++ [right]
              }

            :no ->
              quote do: raise MatchError, term: unquote right
          end

        { block, right_state <~ left_state }

      {:__block__, meta, block} ->
        block =
          Enum.flat_map_reduce(block, state, fn line, state ->
            {line, state} = run(line, state)
            lines =
              case line do
                {:__block__, _, lines} -> lines
                other -> [other]
              end

            {lines, state}
          end)


    end
  end

  def run_guard(ast, state) do
    # TODO
    ast
  end

  def run_match(ast, state, new_state \\ %__MODULE__{}) do
    case ast do
      # Binary matching is special because pinning is not required for variables in types
      {:"<<>>", meta, items} ->
        {items, new_state} =
          Enum.map_reduce(items, new_state, fn
            {:"::", meta, [item, type]}, new_state ->
              # First we just put variables into the type part of the binary match
              {type, _} = run(type, state <~ new_state)

              # And then we get definitions from left side of ::
              {item, new_state} = run_match(item, state, new_state)

              {{:"::", meta, [item, type]}, new_state}

            item, new_state ->
              run_match(item, state, new_state)
          end)

        {{:"<<>>", meta, items}, new_state}

      # When
      {:when, meta, pattern_and_guard} ->
        {guard, patterns} = List.pop_at(pattern_and_guard, -1)
        {patterns, pattern_state} = run_match(patterns, state, new_state)

        # Since guard is not creating new variables, we can just run evaluation against it
        {guard, _guard_state} = run_guard(guard, state <~ pattern_state)

        {{:when, meta, patterns ++ [guard]}, pattern_state}

      # Pin . We handle pin separately before variables
      pin(variable, meta) = pinned when is_variable(variable) ->
        case fetch_bind(state, variable) do
          :error ->
            {pinned, new_state}

          {:ok, val} ->
            {pin(val, meta), new_state}
        end

      # Variable
      variable when is_variable(variable) ->
        {variable, new_state}

      # Map or Tuple or Binary or matchable operator <>
      {n, m, items} when is_list(items) ->
        {items, new_state} = run_match(items, state, new_state)
        {{n, m, items}, new_state}

      # List
      items when is_list(items) ->
        Enum.map_reduce(items, new_state, fn item, acc_state ->
          run_match(item, state, acc_state)
        end)

      # Twople
      {left, right} ->
        {left, new_state} = run_match(left, state, new_state)
        {right, new_state} = run_match(right, state, new_state)

        {{left, right}, new_state}

      # Literal
      literal when is_literal(literal) ->
        {literal, new_state}
    end
  end

  ## State helpers

  defp fetch_bind(%{bindings: bindings}, key) do
    Map.fetch(bindings, unmeta(key))
  end

  defp put_bind(%{bindings: bindings} = state, key, value) do
    key = unmeta(key)
    value = value |> unmeta |> fold(bindings)
    %{state | bindings: Map.put(bindings, key, value)}
  end

  defp fold(value, bindings) do
    postwalk(value, &Map.get(bindings, &1, &1))
  end

  defp left <~ right do
    %{bindings: left_bindings}  = left
    %{bindings: right_bindings} = right

    bindings =
      Map.merge(left_bindings, right_bindings, fn
        # This fn is just a check to assure that contexts do not have conflicts
        _, v, v -> v
        _, _, _ ->
          IO.inspect(left_bindings, label: :left_bindings)
          IO.inspect(right_bindings, label: :right_bindings)
          raise "What the fuck"
      end)

    %{left | bindings: bindings}
  end

end
