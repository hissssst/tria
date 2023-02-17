defmodule Tria.Optimizer.Depmap do

  @moduledoc """
  Special structure for tracking what traits does the optimization rely on
  """

  alias Tria.Language.Analyzer
  alias Tria.Language.FunctionGraph
  alias Tria.Language.MFArity
  import Tria.Language
  import Tria.Language.MFArity, only: [is_dotcall: 1]

  defstruct [
    tria: MapSet.new(),
    safe: MapSet.new(),
    pure: MapSet.new()
  ]

  @type t :: %__MODULE__{
    tria: MapSet.t(MFArity.mfarity()),
    safe: MapSet.t(MFArity.mfarity()),
    pure: MapSet.t(MFArity.mfarity()),
  }

  @type key :: :evaluated | :safe | :pure

  # Guard

  ### Helper guard
  import :erlang, only: [map_get: 2]
  defguardp mapset_empty(mapset) when map_size(map_get(mapset, :map)) == 0

  defguard is_empty(t) when mapset_empty(map_get(t, :safe))
                       and mapset_empty(map_get(t, :pure))
                       and mapset_empty(map_get(t, :evaluated))

  # Public

  @spec put(t(), key(), Tria.t()) :: t()
  def put(depmap, key, value) when key in ~w[safe pure]a do
    check = analyzer_for(key)
    {_, depmap} =
      prewalk(value, depmap, fn
        {:fn, _, _}, depmap ->
          {nil, depmap}

        dotcall, depmap when is_dotcall(dotcall) ->
          mfarity = MFArity.to_mfarity(dotcall)
          if check.(MFArity.to_dotcall mfarity) do
            {dotcall, put_mfarity(depmap, key, mfarity)}
          else
            {dotcall, depmap}
          end

        other, depmap ->
          {other, depmap}
      end)

    depmap
  end
  def put(depmap, :evaluated, dotcall) when is_dotcall(dotcall) do
    put_mfarity(depmap, :tria, MFArity.to_mfarity(dotcall))
  end

  @spec put_many(t(), [key()], Tria.t()) :: t()
  def put_many(depmap, keys, value) do
    Enum.reduce(keys, depmap, &put(&2, &1, value))
  end

  @spec merge(t(), t()) :: t()
  def merge(left, right) do
    Map.merge(left, right, fn
      :__struct__, __MODULE__, __MODULE__ -> __MODULE__
      _k, l, r -> MapSet.union(l, r)
    end)
  end

  @spec reflect_to_graph(MFArity.mfarity(), t()) :: :ok
  def reflect_to_graph(mfarity, %__MODULE__{} = depmap) do
    depmap
    |> Map.from_struct()
    |> Enum.each(fn {_key, values} ->
      Enum.each(values, fn value ->
        FunctionGraph.link(:depends, mfarity, value)
      end)
    end)
  end

  # Helpers

  defp put_mfarity(depmap, key, mfarity) do
    Map.update!(depmap, key, &MapSet.put(&1, mfarity))
  end

  defp analyzer_for(:pure), do: &Analyzer.is_pure/1
  defp analyzer_for(:safe), do: &Analyzer.is_safe/1

end
