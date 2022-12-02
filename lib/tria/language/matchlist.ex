defmodule Tria.Language.Matchlist do

  @moduledoc """
  Structure for working with a set of matches.
  This set of matches is trying to be conflict free
  But some conflicts can't be resolved, so

  #TODO
  This structure is suboptimal as hell, optimizations are needed asap
  """

  import Tria.Language, only: [prewalk: 2, inspect_ast: 2], warn: false

  @type ast :: Tria.t()

  @type match :: {ast(), ast()}

  @typedoc """
  List of matches. Each match is either a map match or a regular match
  """
  @type t :: [match()]

  @doc """
  Checks if this is an ast for map
  """
  defguard is_map_ast(mapast) when
    is_tuple(mapast) and tuple_size(mapast) == 3 and
    :erlang.element(1, mapast) == :"%{}"

  @doc """
  Checks if the specified match is a map_match
  """
  defguard is_map_match(match) when
    is_tuple(match) and tuple_size(match) == 2 and
    is_map_ast(:erlang.element(1, match)) and
    is_map_ast(:erlang.element(2, match))

  defguard is_map_match(left, right) when
    is_map_ast(left) and
    is_map_ast(right)

  defguard is_empty(matchlist) when matchlist == []

  @spec empty() :: t()
  def empty, do: []

  @spec new(list()) :: t()
  def new(list), do: relax(list)

  @doc """
  Puts new map match into the matchlist of overrides previous
  """
  @spec put(t(), ast(), ast()) :: t()
  def put(matchlist, key, value) do
    [{key, value} | matchlist]
  end

  @doc """
  Map.fetch-style of fetching the match value
  """
  @spec fetch(t(), Tria.t()) :: {:ok, ast()} | :error
  def fetch([], _), do: :error
  def fetch([{key, value} | tail], target) do
    if compare(key, target), do: {:ok, value}, else: fetch(tail, target)
  end

  @doc """
  Map.get-style of fetching the match value
  """
  @spec get(t(), ast(), ast()) :: ast()
  def get(matchlist, key, default \\ nil)
  def get([], _, default), do: default
  def get([{key, value} | tail], target, default) do
    if compare(key, target), do: value, else: get(tail, target, default)
  end

  @doc """
  Puts values from matchlist into the ast for all present keys
  """
  @spec fold(Tria.t(), t()) :: Tria.t()
  def fold(ast, matchlist) do
    prewalk(ast, fn x -> get(matchlist, x, x) end)
  end

  @doc """
  Converts matchlist to a list of binds
  """
  @spec to_binds(t(), list()) :: [Tria.t()]
  def to_binds(matchlist, acc \\ [])
  def to_binds([{key, value} | tail], acc) do
    bind = quote do: unquote(key) = unquote(value)
    to_binds(tail, [bind | acc])
  end
  def to_binds([], acc), do: acc

  @doc """
  Converts matchlist to a quoted expression
  """
  @spec to_quoted(t()) :: Tria.t()
  def to_quoted(matchlist) do
    {:__block__, [], to_binds matchlist}
  end

  @doc """
  Merges two matchlists like if they were followed together
  """
  @spec merge(t(), t()) :: t()
  def merge(left, right), do: right ++ left

  @doc """
  Removes duplicates from matchlists. Not sure if this
  function will ever be needed
  """
  @spec relax(t()) :: t()
  # This is O(N^2) operation, so don't call this too often
  def relax(matchlist) do
    matchlist
    |> Enum.reduce(empty(), fn {key, value}, matchlist ->
      case fetch(matchlist, key) do
        {:ok, _value} -> matchlist
        :error -> put(matchlist, key, value)
      end
    end)
    |> :lists.reverse()
  end

  @doc """
  Inspects matchlist
  """
  @spec inspect(t(), Keyword.t()) :: Tria.t()
  def inspect(matchlist, opts \\ []) do
    matchlist
    |> to_quoted()
    |> inspect_ast(opts)
  end

  @doc """
  Inspects diff of two matchlists
  """
  @spec inspect_diff(t(), t(), Keyword.t()) :: :ok
  def inspect_diff(was, became, opts \\ []) do
    IO.puts "#{opts[:label] || "Diff"}:"

    was
    |> List.myers_difference(became)
    |> tap(& match?([], &1) && IO.puts("empty"))
    |> Enum.each(fn
      {:eq, _} ->
        nil

      {:del, {key, value}} ->
        inspect_ast({:=, [], [key, value]}, [label: "-"] ++ opts)

      {:ins, {key, value}} ->
        inspect_ast({:=, [], [key, value]}, [label: "+"] ++ opts)
    end)
  end

  # Helpers

  defp compare([lh | lt], [rh | rt]) do
    compare(lh, rh) && compare(lt, rt)
  end
  defp compare({ll, lr}, {rl, rr}) do
    compare(ll, rl) && compare(lr, rr)
  end
  defp compare({:"%{}", _, l}, {:"%{}", _, r}) do
    compare(Enum.sort(l), Enum.sort(r))
  end
  defp compare({op, _, l}, {op, _, r}), do: compare(l, r)
  defp compare(same, same), do: true
  defp compare(_, _), do: false

end
