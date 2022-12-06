defmodule Tria.Optimizer.Pass.EnumFusion do

  @moduledoc """
  Joining, optimizing and other stuff for Enum

  #TODO translate `reduce` to `:lists.foldl` when it returns list for sure
  #TODO translate `Enum.into(list, %{})` to `Map.new()`
  """

  # Variable `eos` in this module means `enum_or_stream`

  @common_ops ~w[filter reject map reduce sum count product frequencies flat_map foldl foldr]a
  @list_returning_op ~w[filter reject map concat]a

  import Tria.Language
  import Tria.Language.Tri
  import Tria.Language.Analyzer, only: [is_pure: 1]
  alias Tria.Optimizer.Pass.Evaluation
  alias Tria.Compiler.SSATranslator

  defguard is_fusable(module, function, arity)
           when module == Enum
           or module == Stream
           or (module == Map and function == :new)

  def run_once!(ast, _opts \\ []) do
    {argument, steps} = unchain(ast)

    steps =
      steps
      |> unreject()
      |> unstream()
      |> join_consecutive()
      |> into_to_map_new()

    chain(argument, steps)
  end

  ### Converts `reject` to `filter`

  def unreject([{eos, :reject, [the_fn]} | tail]) do
    body = evaluate tri fn x -> ! the_fn.(x) end
    [{eos, :filter, [body]} | unreject(tail)]
  end
  def unreject([head | tail]), do: [head | unreject(tail)]
  def unreject([]), do: []

  ### Removing Stream before Enum

  def unstream([{Stream, op, args}, {Enum, :to_list, []} | tail]) when op in ~w[map filter]a do
    [{Enum, op, args} | unstream(tail)]
  end
  def unstream([head | tail]), do: [head | unstream(tail)]
  def unstream([]), do: []

  ### Converts some operations to reduce

  def reducify([step | tail]) do
    case step do
      {eos, :sum, []} ->
        body = tri fn x, acc -> x + acc end
        [{eos, :reduce, [0, body]} | reducify(tail)]

      {eos, :count, []} ->
        body = tri fn _, acc -> acc + 1 end
        [{eos, :reduce, [0, body]} | reducify(tail)]

      {eos, :product, []} ->
        body = tri fn _, acc -> acc * 1 end
        [{eos, :reduce, [1, body]} | reducify(tail)]

      {eos, :frequencies, []} ->
        body = tri fn x, acc -> Map.update(acc, x, 1, fn y -> y + 1 end) end
        [{eos, :reduce, [tri(%{}), body]} | reducify(tail)]

      {eos, :all?, []} ->
        body = tri fn x, acc -> Map.update(acc, x, 1, fn y -> y + 1 end) end
        [{eos, :reduce, [true, body]} | reducify(tail)]

      _ ->
        [step | reducify(tail)]
    end
  end

  ### Joining of two same calls

  ### zip + reduce to zip_reduce
  def join_consecutive([{_, :zip, []}, {Enum, :reduce, [acc, reducer]} | tail]) do
    [{Enum, :zip_reduce, [acc, reducer]} | join_consecutive(tail)]
  end

  def

  ### Pairs of mappers, filters etc.
  def join_consecutive([{eos, left_op, [left]}, {eos, right_op, [right]} | tail]) do
    if is_pure(left) or is_pure(right) do
      case {left_op, right_op} do
        # Same ops
        {:filter, :filter} ->
          body = evaluate tri fn x -> left.(x) && right.(x) end
          [{eos, :filter, [body]} | join_consecutive(tail)]

        {:map, :map} ->
          body = evaluate tri fn x -> right.(left.(x)) end
          [{eos, :map, [body]} | join_consecutive(tail)]

        {:flat_map, :flat_map} ->
          body = evaluate tri fn x -> Enum.flat_map(left.(x), right) end
          [{eos, :flat_map, [body]} | join_consecutive(tail)]

        {:drop, :drop} ->
          # I don't think this optimization hits at least once
          body = evaluate tri fn x -> Enum.drop(x, left + right) end
          [{eos, :drop, [body]} | join_consecutive(tail)]

        # Each
        {:map, :each} ->
          body = evaluate tri fn x -> right.(left.(x)) end
          [{eos, :each, [body]} | join_consecutive(tail)]

        {:filter, :each} ->
          body = evaluate tri fn x -> if left.(x), do: right.(x) end
          [{eos, :each, [body]} | join_consecutive(tail)]

        _ ->
          [{eos, left_op, [left]}, {Enum, right_op, [right]} | join_consecutive(tail)]
      end
    else
      [{eos, left_op, [left]}, {eos, right_op, [right]} | join_consecutive(tail)]
    end
  end

  ### Joining with reduce
  def join_consecutive([{eos, op, [func]}, {Enum, :reduce, [accumulator, reducer]} | tail]) do
    if is_pure(accumulator) and (is_pure(func) or is_pure(reducer)) do
      case op do
        :map ->
          body = evaluate tri fn x, acc -> reducer.(func.(x), acc) end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        :filter ->
          body = evaluate tri fn x, acc -> if func.(x), do: reducer.(x, acc), else: acc end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        :flat_map ->
          body = evaluate tri fn x, acc -> Enum.reduce(x, acc, reducer) end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        _ ->
          [{eos, op, [func]}, {Enum, :reduce, [accumulator, reducer]} | join_consecutive(tail)]
      end
    else
      [{eos, op, [func]}, {Enum, :reduce, [accumulator, reducer]} | join_consecutive(tail)]
    end
  end
  def join_consecutive([{_eos, :concat, []}, {Enum, :reduce, [accumulator, reducer]} | tail]) do
    body = evaluate tri fn x, acc -> Enum.reduce(x, acc, reducer) end
    [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]
  end

  ### to_list
  def join_consecutive([{Enum, :to_list, []}, {eos, op, [arg]} | tail]) when op in ~w[map filter flat_map each]a do
    [{eos, op, [arg]} | join_consecutive(tail)]
  end

  ### Other
  def join_consecutive([{eos, left_op, [func], {Enum, right_op, []}}]) do
    case {left_op, right_op} do
      {:filter, :empty?} ->
        if is_pure(func) do
          [{Enum, :find, [func]}]
        else
          [{Enum, :filter, [func], {Enum, :empty?, []}}]
        end

      _ ->
        [{eos, left_op, [func], {Enum, right_op, []}}]
    end
  end
  def join_consecutive([_ | tail]), do: unreject(tail)
  def join_consecutive([]), do: []

  ### Enum.into(_, %{}) to Map.new

  def into_to_map_new([{Enum, :into, [tri(%{})]} | tail]) do
    [{Map, :new, []} | into_to_map_new(tail)]
  end
  def into_to_map_new([{Enum, :into, [tri(%{})]} | tail]) do
    [{Map, :new, []} | into_to_map_new(tail)]
  end
  def into_to_map_new([head | tail]), do: [head | into_to_map_new(tail)]
  def into_to_map_new([]), do: []

  ### Chaining and unchaining

  defp unchain(chain, acc \\ [])
  defp unchain(dot_call(eos, function, args), acc) when is_fusable(eos, function, length(args)) do
    case {function, args} do
      {common_op, [subj | args]} when common_op in @common_ops ->
        unchain(subj, [{eos, common_op, args} | acc])

      _ ->
        raise "Not implemented"
    end
  end
  defp unchain(other, acc), do: {other, acc}

  defp chain(arg, [{module, function, args} | tail]) do
    module
    |> dot_call(function, [arg | args])
    |> chain(tail)
  end
  defp chain(arg, []), do: arg

  defp evaluate(ast) do
    ast
    |> SSATranslator.from_tria()
    |> Evaluation.run_once!()
  end

end
