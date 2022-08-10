defmodule Tria.Pass.EnumOptimizer do
  @moduledoc """
  Optimizes Enum chains into one big loop function
  """

  @common_ops ~w[filter reject map]a

  import Tria.Common
  alias Tria.Pass.Evaluation

  defguardp is_filject(x) when x in ~w[filter reject]a

  def run_once(chain, opts \\ []) do
    {arg, steps} = unchain(chain)

    steps =
      steps
      |> IO.inspect()
      |> general()
      |> unstream()
      |> zip_maps()
      |> zip_filters()

    case map_unroll(steps, arg, Keyword.get(opts, :max_unroll_size, 16)) do
      {:ok, unrolled} ->
        unrolled

      :error ->
        steps
        |> rechain(arg)
    end
    |> inspect_ast()
  end

  def test do
    alias Tria.Translator.Elixir, as: ElixirTranslator

    {:ok, quoted, _env} =
      quote do
        list
        |> Stream.map(fn x -> x + 99999 end)
        |> Enum.map(fn x -> x + 1 end)
        |> Enum.map(fn x -> x + 2 end)
        |> Enum.map(fn x -> x + 3 end)
      end
      |> ElixirTranslator.to_tria(__ENV__)

    run_once(quoted)
  end

  # Optimizers

  ## Performs really simple optimizations for Enum

  ### map, join to map_join
  defp general([{Enum, :map, [func]}, {Enum, :join, joiner_or_default} | tail]) do
    [{Enum, :map_join, joiner_or_default ++ [func]} | tail]
  end

  ### map, reduce to map_reduce
  defp general([{Enum, :map, [func]}, {Enum, :reduce, [acc, reducer]} | tail]) do
    reducer =
      quote do
        fn item, acc ->
          new_item = unquote(func).(item)
          new_acc = unquote(reducer).(new_item, acc)
          {new_item, new_acc}
        end
      end
      |> Evaluation.run_once!()

    [{Enum, :map_reduce, [acc, reducer]} | tail]
  end

  ### fallback
  defp general([other | tail]) do
    [other | general(tail)]
  end

  ### Zips consecutive maps

  defp zip_maps(steps, acc \\ nil)

  defp zip_maps([{mod, :map, [func]}, {mod, :map, _} = next_item | tail], nil) do
    var = {:x, [], gen_uniq_context()}
    acc = {mod, var, Evaluation.run_once! quote(do: unquote(func).(unquote(var)))}
    zip_maps([next_item | tail], acc)
  end

  defp zip_maps([{mod, :map, [new_func]} | tail], {mod, var, body}) do
    acc = {mod, var, Evaluation.run_once! quote(do: unquote(new_func).(unquote(body)))}
    zip_maps(tail, acc)
  end

  defp zip_maps([other | tail], nil) do
    [other | zip_maps(tail, nil)]
  end

  defp zip_maps(tail, {mod, var, body}) do
    func = quote(do: fn unquote(var) -> unquote(body) end)
    [{mod, :map, [func]} | zip_maps(tail, nil)]
  end

  defp zip_maps([], _), do: []

  ### Zips consecutive filters and rejects

  defp zip_filters(steps, acc \\ nil)

  defp zip_filters([{mod, filject, [func]}, {mod, next_filject, _} = next_item | tail], nil)
       when is_filject(filject) and is_filject(next_filject) do
    var = {:x, [], gen_uniq_context()}
    acc = {mod, var, filterify(filject, quote(do: unquote(func).(unquote(var))))}
    zip_filters([next_item | tail], acc)
  end

  defp zip_filters([{mod, filject, [new_func]} | tail], {mod, var, left}) when is_filject(filject) do
    right = filterify(filject, quote(do: unquote(new_func).(unquote(var))))
    acc = {mod, var, quote(do: unquote(left) && unquote(right))}
    zip_filters(tail, acc)
  end

  defp zip_filters([other | tail], nil) do
    [other | zip_filters(tail, nil)]
  end

  defp zip_filters(tail, {mod, var, body}) do
    func = quote(do: fn unquote(var) -> unquote(body) end)
    [{mod, :filter, [func]} | zip_filters(tail, nil)]
  end

  defp zip_filters([], _), do: []

  defp filterify(:filter, code), do: Evaluation.run_once! code
  defp filterify(:reject, code), do: Evaluation.run_once! quote(do: !unquote(code))

  ### Enum.map unrolling

  defp map_unroll([{:"Elixir.Enum", :map, [func]}], arg, max_unroll_size) when is_list(arg) do
    if length(arg) > max_unroll_size do
      :error
    else
      unrolled =
        Enum.map(arg, fn item ->
          Evaluation.run_once! quote(do: unquote(func).(unquote(item)))
        end)

      {:ok, unrolled}
    end
  end
  defp map_unroll(_other_chain, _arg, _max_unroll_size), do: :error

  # Helpers

  defp unchain(chain, acc \\ [])

  defp unchain(dot_call(Enum, function, args), acc) do
    case {function, args} do
      {common_op, [subj | args]} when common_op in @common_ops ->
        unchain(subj, [{Enum, common_op, args} | acc])

      _ ->
        raise "Not implemented"
    end
  end
  defp unchain(dot_call(Stream, function, args), acc) do
    case {function, args} do
      {common_op, [subj | args]} when common_op in @common_ops ->
        unchain(subj, [{Stream, common_op, args} | acc])

      _ ->
        raise "Not implemented"
    end
  end

  defp unchain(other, acc), do: {other, acc}

  defp rechain(operations, arg) do
    Enum.reduce(operations, arg, fn {mod, op, args}, arg ->
      dot_call(mod, op, [arg | args])
    end)
  end

  defp unstream(chain) do
    chain
    |> Enum.reverse()
    |> Enum.reduce({[], false}, fn
      {Stream, _, _} = step, {items, false} ->
        {[step | items], false}
        
      {Stream, op, args}, {items, true} when op in @common_ops ->
        {[{Enum, op, args} | items], true}

      {Enum, _, _} = step, {items, _} ->
        {[step | items], true}

      other_step, {items, acc} ->
        {[other_step | items], acc}
    end)
    |> elem(0)
  end
end
