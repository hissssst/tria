defmodule Tria.Language.Bindmap do

  @moduledoc """
  Utility structure for working with bindings map
  """

  @type t :: %{Tria.t() => Tria.t()}

  @type key :: Tria.t()

  @type value :: Tria.t()

  @typedoc """
  Function which decides whether the unfolding should continue or not
  """
  @type predicate :: (Tria.t() -> :cont | :skip | :halt)

  @type unfold_option :: {:while, predicate()}

  import Tria.Language
  import Tria.Language.Meta, only: [unmeta: 1]

  def new(), do: %{}

  @doc """
  Checks if bindmap contains no bindings
  """
  defguard is_empty(bindmap) when map_size(bindmap) == 0

  @spec put(t(), key(), value()) :: t()
  def put(bindmap, key, value) do
    Map.put(bindmap, key, value)
  end

  @spec put(t(), [{key(), value()}]) :: t()
  def put(bindmap, pairs) do
    Enum.reduce(pairs, bindmap, fn {key, value}, bindmap -> put(bindmap, key, value) end)
  end

  def merge!(left, right) do
    Map.merge(left, right, fn
      _, v, v -> v
      k, l, r -> raise ArgumentError, message: "Bindmaps have a conflicting key #{inspect k} with #{inspect l} vs #{inspect r}"
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

  # @spec unfold_once(Tria.t(), t()) ::
  # def unfold_once(value, bindmap) do

  # end

  @spec unfold(Tria.t(), t(), [unfold_option()]) :: Tria.t()
  def unfold(value, bindmap, opts \\ [])
  def unfold(value, bindmap, []) do
    postwalk(value, &Map.get(bindmap, unmeta(&1), &1))
  end

  def unfold(value, bindmap, while: predicate) do
    unfold_while(value, bindmap, predicate)
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
