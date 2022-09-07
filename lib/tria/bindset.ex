defmodule Tria.Matchset do

  @moduledoc """
  Structure for working with a set of matches.
  This set of matches is trying to be conflict free
  But some conflicts can't be resolved, so
  """

  import Tria.Common
  alias Tria.Ternary
  use Ternary
  alias Tria.Interpreter

  defstruct [
    map_matches: [], # Map matchings
    # # level: :yes,     # Ternary level of if this match will match
    matches: %{}     # Regular matchings
  ]

  @type map_match :: {[{Macro.t(), Macro.t()}], [{Macro.t(), Macro.t()}]}

  @type t :: %__MODULE__{
    # level: Ternary.t(),
    map_matches: [map_match()],
    matches: %{Macro.t() => Macro.t()}
  }

  @doc """
  Puts new map match into the matchset of overrides previous
  """
  @spec put(t(), map_match()) :: t()
  def put(%{map_matches: map_matches} = matchset, map_match) do
    # Since map_match is a twople of lists of twoples, we can safely use regular fold
    map_match = fold(map_match, matchset)
    %{matchset | map_matches: [map_match | map_matches]}
  end

  @doc """
  Puts new match into the matchset of overrides previous
  """
  @spec put(t(), Macro.t(), Macro.t()) :: t()
  def put(%{matches: matches} = matchset, key, value) do
    value = fold(value, matchset)
    %{matchset | matches: Map.put(matches, key, value)}
  end

  @doc """
  Adds compatible information about map match to the matchset about the match
  """
  @spec precise(t(), map_match()) :: Ternary.t(t())
  def precise(matchset, {lefts, rights}) do
    left_with_matches = for left <- lefts, do: {left, fetch_all(matchset, left)}
    right_with_matches = for right <- rights, do: {right, fetch_all(matchset, right)}

    Enum.reduce(:yes, )
      
    catch :no -> :no
  end

  @doc """
  Adds compatible information to the matchset about the match
  """
  @spec precise(t(), Macro.t(), Macro.t()) :: Ternary.t(t())
  def precise(matchset, key, value) do
    level =
      matchset
      |> fetch_all(key)
      |> Enum.reduce(:yes, fn existing, acc ->
        case Interpreter.mutual_match(existing, value) do
          :no -> throw :no
          level -> level && acc
        end
      end)

    {level, put(matchset, key, value)}
    catch :no -> :no
  end

  @doc """
  Map.get style of taking the match value
  """
  @spec get(t(), Macro.t(), default :: any()) :: Macro.t() | any()
  def get(%{matches: matches}, key, default), do: Map.get(matches, key, default)

  @doc """
  Map.fetch-style of fetching the match value
  """
  @spec fetch(t(), Macro.t()) :: {:ok, Macro.t()} | :error
  def fetch(%{matches: matches}, key), do: Map.fetch(matches, key)

  @doc """
  Puts values from matchset into the ast
  """
  @spec fold(Macro.t(), t()) :: Macro.t()
  def fold(ast, matchset) do
    #TODO fold from map_matches
    Macro.prewalk(ast, fn x -> get(matchset, x, x) end)
  end

  @doc """
  Just overrides the first matches
  """
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
        Enum.any()
      end)
  end

  # Optimize the 99%
  def fetch_all(%{map_matches: []} = matchset, key) do
    case fetch(matchset, key) do
      {:ok, value} -> [value]
      :error -> []
    end
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

  # defp keys([]), do: []
  # defp keys([{key, _} | tail]), do: [key | keys(tail)]

  # defp values([]), do: []
  # defp values([{_, value} | tail]), do: [value | values(tail)]

end
