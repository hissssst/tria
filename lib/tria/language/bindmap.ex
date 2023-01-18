defmodule Tria.Language.Bindmap do

  @moduledoc """
  Utility structure for working with bindings map
  """

  @typedoc """
  A bindmap is a `map()` of bindings
  where key is on the left side of `=`
  and value is on the right
  """
  @type t :: %{Tria.t() => Tria.t()}

  @type key :: Tria.t()

  @type value :: Tria.t()

  @typedoc """
  Function which decides whether the unfolding should continue or not
  """
  @type predicate :: (Tria.t() -> :cont | :skip | :halt)

  @type unfold_option :: {:while, predicate()} | {:context, Tria.Language.context()}

  import Tria.Language
  import Tria.Language.Guard
  import Tria.Language.Meta, only: [unmeta: 1]

  alias Tria.Language.Matchlist

  @doc """
  Checks if bindmap contains no bindings
  """
  defguard is_empty(bindmap) when map_size(bindmap) == 0

  def new(), do: %{}

  @spec put(t(), key(), value()) :: t()
  def put(bindmap, key, value) do
    Map.put(bindmap, unmeta(key), value)
  end

  @spec put(t(), [{key(), value()}]) :: t()
  def put(bindmap, pairs) do
    Enum.reduce(pairs, bindmap, fn {key, value}, bindmap -> put(bindmap, key, value) end)
  end

  def merge!(left, right) do
    Map.merge(left, right, fn
      _, v, v -> v
      k, l, r ->
        if unmeta(l) != unmeta(r) do
          raise ArgumentError, message: "Bindmaps have a conflicting key"
          <> " #{inspect k, pretty: true}"
          <> " with #{ast_to_string l} vs #{ast_to_string r}"
        else
          l
        end
    end)
  end

  @spec fetch(t(), key()) :: {:ok, value()} | :error
  def fetch(bindmap, key) do
    Map.fetch(bindmap, unmeta key)
  end

  @spec fetch_unfolded(t(), key(), Keyword.t()) :: {:ok, value()} | :error
  def fetch_unfolded(bindmap, key, opts \\ []) do
    with {:ok, value} <- fetch(bindmap, key) do
      {:ok, unfold(value, bindmap, opts)}
    end
  end

  @spec merge_matchlist(t(), Matchlist.t()) :: t()
  def merge_matchlist(bindmap, matchlist) do
    matchlist
    |> Enum.reverse()
    |> Enum.reduce(bindmap, fn {key, value}, bindmap ->
      put(bindmap, key, value)
    end)
  end

  # @spec unfold_once(Tria.t(), t()) ::
  # def unfold_once(value, bindmap) do

  # end

  @spec unfold(Tria.t(), t(), [unfold_option()]) :: Tria.t()
  def unfold(value, bindmap, opts \\ [])
  def unfold(value, bindmap, while: predicate) do
    unfold_while(value, bindmap, predicate)
  end
  def unfold(value, bindmap, opts) do
    context_prewalk(value, fn
      ast, nil ->
        Map.get(bindmap, ast, ast)

      ast, :guard ->
        new = Map.get(bindmap, ast, ast)
        if is_guard(new) do
          new
        else
          ast
        end

      pin(ast), :match ->
        case fetch(bindmap, ast) do
          {:ok, new} ->
            cond do
              findwalk(new, &match?({:%{}, _, _}, &1)) ->
                ast

              quoted_literal?(new) ->
                new

              vared_literal?(new) ->
                postwalk(new, fn
                  v when is_variable(v) -> pin(v)
                  other -> other
                end)

              true ->
                ast
            end

          :error ->
            ast
        end

      other, :match ->
        other
    end, opts[:context])
  end

  @spec unfold_while(Tria.t(), t(), predicate()) :: Tria.t()
  def unfold_while(value, bindmap, predicate) do
    f = fn x -> unfold_while(x, bindmap, predicate) end
    unfolded = Map.get(bindmap, unmeta(value), value)
    case predicate.(unfolded) do
      :cont ->
        case unfolded do
          {left, right} ->
            {f.(left), f.(right)}

          [head | tail] ->
            [f.(head) | f.(tail)]

          {node, meta, children} ->
            {f.(node), meta, f.(children)}

          other ->
            other
        end

      :skip ->
        case value do
          {left, right} ->
            {f.(left), f.(right)}

          [head | tail] ->
            [f.(head) | f.(tail)]

          {node, meta, children} ->
            {f.(node), meta, f.(children)}

          other ->
            other
        end

      :halt ->
        value
    end
  end

end
