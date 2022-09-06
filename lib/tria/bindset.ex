defmodule Tria.Matchset do

  @moduledoc """
  Structure for working with a set of matches
  """

  alias Tria.Interpreter
  alias Tria.Ternary

  defstruct [
    map_matches: [], # Map matchings
    # # level: :yes,     # Ternary level of if this match will match
    matches: %{}     # Regular matchings
  ]

  @type map_match :: {[{Macro.t(), Macro.t()}], [{Macro.t(), Macro.t()}]}

  @type t :: %__MODULE__{
    level: Ternary.t(),
    map_matches: [map_match()],
    matches: %{Macro.t() => Macro.t()}
  }

  @spec put_map(t(), map_match()) :: t()
  def put_map(%{map_matches: map_matches} = matchset, map_match) do
    # Since map_match is a twople of lists of twoples, we can safely use regular fold
    map_match = fold(map_match, matchset)
    %{matchset | map_matches: [map_match | map_matches]}
  end

  @spec put(t(), Macro.t(), Macro.t()) :: t()
  def put(%{matches: matches} = matchset, key, value) do
    value = fold(value, matchset)
    %{matchset | matches: Map.put(matches, key, value)}
  end

  @doc "Map.get style of taking the match value"
  @spec get(t(), Macro.t(), default :: any()) :: Macro.t() | any()
  def get(%{matches: matches}, key, default), do: Map.get(matches, key, default)

  @doc "Map.fetch-style of fetching the match value"
  @spec fetch(t(), Macro.t()) :: {:ok, Macro.t()} | :error
  def fetch(%{matches: matches}, key), do: Map.fetch(matches, key)

  @doc "Puts values from matchset into the ast"
  @spec fold(Macro.t(), t()) :: Macro.t()
  def fold(ast, matchset) do
    #TODO fold from map_matches
    Macro.prewalk(ast, fn x -> get(matchset, x, x) end)
  end

  def merge(matchset, override) do
    matchset =
      Enum.reduce(override.matches, matchset, fn {left, right}, acc ->
        put(acc, left, right)
      end)

    matchset =
      Enum.reduce(override.map_matches, matchset, fn map_match, acc ->
        put_map(acc, map_match)
      end)

    matchset
  end

  def join(left, right) do
    matches =
      Enum.reduce(right.matches, left.matches, fn {key, value}, acc ->
        Enum.any?()
      end)
  end

  def fetch_all(matchset, key) do
    case fetch(matchset, key) do
      {:ok, value} ->
        [value]

      :error ->
        matchset.map_matches
        |> Enum.reduce(MapSet.new(), fn {lefts, rights}, acc ->
          acc
          |> MapSet.intersection(MapSet.new(if(key in lefts, do: rights, else: [])))
          |> MapSet.intersection(MapSet.new(if(key in rights, do: lefts, else: [])))
        end)
        |> MapSet.to_list()
    end
  end

  # Here I use counters, but all of this will be dropped later
  defp mutual_translate({left, right}) do
    {left, translations} =
      Macro.prewalk(left, %{}, fn
        {_, _, args} = ast, translations when is_list(args) ->
          if is_special_form(ast) do
            {ast, translations}
          else
            var = {:var, [counter: :erlang.unique_integer()], nil}
            {var, Map.put(translations, ast, var)}
          end

        other, translations ->
          {other, translations}
      end)

    right =
      Macro.prewalk(right, fn
        {_, _, args} = ast when is_list(args) ->
          if is_special_form(ast) do
            ast
          else
            Map.get_lazy(translations, ast, fn ->
              {:var, [counter: :erlang.unique_integer()], nil}
            end)
          end

        other ->
          other
      end)

    {left, right}
  end

  # defp keys([]), do: []
  # defp keys([{key, _} | tail]), do: [key | keys(tail)]

  # defp values([]), do: []
  # defp values([{_, value} | tail]), do: [value | values(tail)]

end
