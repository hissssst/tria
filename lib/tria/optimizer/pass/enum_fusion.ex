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
  alias Tria.Language.Analyzer
  alias Tria.Optimizer.Pass.Evaluation
  alias Tria.Compiler.SSATranslator

  defguard is_fusable(module, function, arity)
           when module == Enum or module == Stream
           or (module == Map and function == :new)
           or (module == :lists and function == :reverse and arity == 1)

  @tri_opts meta: false

  def run_once!(ast, opts \\ []) do
    {:ok, ast} = run_once(ast, opts)
    ast
  end

  def run_while(ast, opts \\ []) do
    {argument, steps} = unchain(ast)
    steps = do_run_while(steps, opts)
    chain(argument, steps)
  end

  defp do_run_while(steps, opts) do
    case run_pipeline(steps, opts) do
      {steps, true} ->
        do_run_while(steps, opts)

      {steps, false} ->
        # Here I use `steps` from `run_pipeline` to avoid copying
        # Because, when no optimization is hit, there's nothing changed
        steps
    end
  end

  def run_once(ast, opts \\ []) do
    {argument, steps} = unchain(ast)
    case run_pipeline(steps, opts) do
      {steps, true} ->
        {:ok, chain(argument, steps)}

      {_, false} ->
        {:error, :nothing_to_fuse}
    end
  end

  defp run_pipeline(steps, _opts) do
    at_hit fn ->
      steps
      |> reducify()
      |> unreject()
      |> unstream()
      |> join_consecutive()
      |> into_to_map_new()
    end
  end

  ### Converts `reject` to `filter`

  def unreject([{eos, :reject, [the_fn]} | tail]) do
    hit()
    body = evaluate tri fn x -> ! the_fn.(x) end
    [{eos, :filter, [body]} | unreject(tail)]
  end
  def unreject([head | tail]), do: [head | unreject(tail)]
  def unreject([]), do: []

  ### Removing Stream before Enum

  def unstream([{Stream, op, args}, {Enum, :to_list, []} | tail]) when op in ~w[map filter]a do
    hit()
    [{Enum, op, args} | unstream(tail)]
  end
  def unstream([head | tail]), do: [head | unstream(tail)]
  def unstream([]), do: []

  ### Converts some operations to reduce

  def reducify([step | tail]) do
    case step do
      {eos, :sum, []} ->
        hit()
        body = tri fn x, acc -> x + acc end
        [{eos, :reduce, [0, body]} | reducify(tail)]

      {eos, :count, []} ->
        hit()
        body = tri fn _, acc -> acc + 1 end
        [{eos, :reduce, [0, body]} | reducify(tail)]

      {eos, :product, []} ->
        hit()
        body = tri fn x, acc -> x * acc end
        [{eos, :reduce, [1, body]} | reducify(tail)]

      {eos, :frequencies, []} ->
        hit()
        body = tri fn x, acc -> Map.update(acc, x, 1, fn y -> y + 1 end) end
        [{eos, :reduce, [tri(%{}), body]} | reducify(tail)]

      _ ->
        [step | reducify(tail)]
    end
  end
  def reducify([]), do: []

  ### Joining of two same calls

  ### zip + reduce to zip_reduce
  def join_consecutive([{_, :zip, []}, {Enum, :reduce, [acc, reducer]} | tail]) do
    hit()
    [{Enum, :zip_reduce, [acc, reducer]} | join_consecutive(tail)]
  end

  ### Pairs of mappers, filters etc.
  def join_consecutive([{left_eos, left_op, [left]}, {right_eos, right_op, [right]} | tail]) do
    if left_eos == Stream or is_pure(left) or is_pure(right) do
      case {left_op, right_op} do
        # Same ops
        {:filter, :filter} ->
          hit()
          body = evaluate tri fn x -> left.(x) && right.(x) end
          [{right_eos, :filter, [body]} | join_consecutive(tail)]

        {:map, :map} ->
          hit()
          body = evaluate tri fn x -> right.(left.(x)) end
          [{right_eos, :map, [body]} | join_consecutive(tail)]

        {:flat_map, :flat_map} ->
          hit()
          body = evaluate tri fn x -> Enum.flat_map(left.(x), right) end
          [{right_eos, :flat_map, [body]} | join_consecutive(tail)]

        {:drop, :drop} ->
          hit()
          # I don't think this optimization hits at least once
          body = evaluate tri fn x -> Enum.drop(x, left + right) end
          [{right_eos, :drop, [body]} | join_consecutive(tail)]

        # Each
        {:map, :each} ->
          hit()
          body = evaluate tri fn x -> right.(left.(x)) end
          [{right_eos, :each, [body]} | join_consecutive(tail)]

        {:filter, :each} ->
          hit()
          body = evaluate tri fn x -> if left.(x), do: right.(x) end
          [{right_eos, :each, [body]} | join_consecutive(tail)]

        _ ->
          [{left_eos, left_op, [left]} | join_consecutive([{right_eos, right_op, [right]} | tail])]
      end
    else
      [{left_eos, left_op, [left]} | join_consecutive([{right_eos, right_op, [right]} | tail])]
    end
  end

  ### Joining with reduce
  def join_consecutive([{eos, op, [func]}, {Enum, :reduce, [accumulator, reducer]} = step | tail]) do
    if is_pure(accumulator) and (is_pure(func) or is_pure(reducer)) do
      case op do
        :map ->
          hit()
          body = evaluate tri fn x, acc -> reducer.(func.(x), acc) end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        :filter ->
          hit()
          body = evaluate tri fn x, acc -> if func.(x), do: reducer.(x, acc), else: acc end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        :flat_map ->
          hit()
          body = evaluate tri fn x, acc -> Enum.reduce(x, acc, reducer) end
          [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]

        _ ->
          [{eos, op, [func]} | join_consecutive([step | tail])]
      end
    else
      [{eos, op, [func]} | join_consecutive([step | tail])]
    end
  end
  def join_consecutive([{_eos, :concat, []}, {Enum, :reduce, [accumulator, reducer]} | tail]) do
    hit()
    body = evaluate tri fn x, acc -> Enum.reduce(x, acc, reducer) end
    [{Enum, :reduce, [accumulator, body]} | join_consecutive(tail)]
  end

  ### to_list
  def join_consecutive([{Enum, :to_list, []}, {eos, op, [arg]} | tail]) when op in ~w[map filter flat_map each]a do
    hit()
    [{eos, op, [arg]} | join_consecutive(tail)]
  end

  ### Elixir's `for` optimization
  def join_consecutive([{Enum, :reduce, [[], reducer]} = reduce, {:lists, :reverse, []} = reverse | tail]) do
    case reducer do
      tri(fn x, acc -> [body | acc] end) ->
        hit()
        [{Enum, :map, [tri(fn x -> body end)]} | join_consecutive(tail)]

      _ ->
        [reduce | join_consecutive([reverse | tail])]
    end
  end

  ### Last steps
  # def join_consecutive([{eos, left_op, [func], {Enum, right_op, []}}]) do
  #   case {left_op, right_op} do
  #     {:filter, :empty?} ->
  #       hit()
  #       if is_pure(func) do
  #         [{Enum, :find, [func]}]
  #       else
  #         [{Enum, :filter, [func], {Enum, :empty?, []}}]
  #       end

  #     _ ->
  #       [{eos, left_op, [func], {Enum, right_op, []}}]
  #   end
  # end
  def join_consecutive([head | tail]), do: [head | join_consecutive(tail)]
  def join_consecutive([]), do: []

  ### Enum.into(_, %{}) to Map.new

  def into_to_map_new([{Enum, :map, [func]}, {Enum, :into, [tri(%{})]} | tail]) do
    hit()
    [{Map, :new, [func]} | into_to_map_new(tail)]
  end
  def into_to_map_new([{Enum, :into, [tri(%{}), func]} | tail]) do
    hit()
    [{Map, :new, [func]} | into_to_map_new(tail)]
  end
  def into_to_map_new([{Enum, :into, [tri(%{})]} | tail]) do
    hit()
    [{Map, :new, []} | into_to_map_new(tail)]
  end
  def into_to_map_new([head | tail]), do: [head | into_to_map_new(tail)]
  def into_to_map_new([]), do: []

  ### Chaining and unchaining

  defp unchain(chain, acc \\ [])
  defp unchain(dot_call(eos, function, [subj | args]), acc) when is_fusable(eos, function, length(args) + 1) do
    unchain(subj, [{eos, function, args} | acc])
  end
  defp unchain(other, acc), do: {other, acc}

  defp chain(arg, [{module, function, args} | tail]) do
    module
    |> dot_call(function, [arg | args])
    |> chain(tail)
  end
  defp chain(arg, []), do: arg

  ### Evalution and Analysis helper

  defp evaluate(ast) do
    ast
    |> SSATranslator.from_tria!()
    |> Evaluation.run_while()
  end

  defp is_pure({:fn, _, [clauses]}) do
    Analyzer.is_pure(clauses)
  end
  defp is_pure(other), do: Analyzer.is_pure(other)

  ### Hitting and stuff

  defp hit, do: Process.put(:hit, true)
  defp at_hit(func) do
    old = Process.get(:hit)
    try do
      {func.(), Process.get(:hit, false)}
    after
      old && Process.put(:hit, old) || Process.delete(:hit)
    end
  end

end
