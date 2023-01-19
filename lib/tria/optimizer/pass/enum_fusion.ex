defmodule Tria.Optimizer.Pass.EnumFusion do

  @moduledoc """
  Joining, optimizing and other stuff for Enum

  #TODO fusable check
  #TODO translate enum functions to `:lists` where applicable
  """

  # `eos` in this module means `enum_or_stream`

  use Tria.Optimizer.Pass

  import Tria.Language
  import Tria.Language.Tri
  import Tria.Language.Analyzer, only: [is_pure: 1]
  alias Tria.Optimizer.Pass
  alias Tria.Optimizer.Pass.Evaluation
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer

  #@common_ops ~w[filter reject map reduce sum count product frequencies flat_map foldl foldr]a
  #@list_returning_op ~w[filter reject map concat]a

  defguard is_fusable(module, function, arity)
           when module == Enum or module == Stream
           or (module == Map and function == :new)
           or (module == :lists and function == :reverse and arity == 1)

  defguard is_eos(eos) when eos in [Enum, Stream]

  @tri_opts meta: false

  defmacrop hit do
    line = __CALLER__.line
    quote do
      Tracer.tag(unquote(line), label: :fusion_hit_at)
      Process.put(:hit, true)
    end
  end

  ## Pass callbacks

  def begin(ast, _opts) do
    {:ok, unchain(ast)}
  end

  def run_once({argument, steps}, opts) do
    case run_pipeline(steps, opts) do
      {steps, true} ->
        {:ok, {argument, steps}}

      {_, false} ->
        {:error, :nothing_to_optimize}
    end
  end

  def finish({argument, steps}, _opts) do
    {:ok, chain(argument, steps)}
  end

  def run_while(ast, opts \\ []) do
    postwalk(ast, fn
      dot_call(module, function, args) = ast when is_fusable(module, function, length(args)) ->
        case Pass.run_until(__MODULE__, ast, opts) do
          {:ok, result} ->
            result

          _ ->
            ast
        end

      other ->
        other
    end)
  end

  defp run_pipeline(steps, _opts) do
    with_pdict [hit: false], fn ->
      steps  =
        steps
        |> reducify()
        |> unreject()
        |> unstream()
        |> join_consecutive()
        |> into_to_map_new()

      {steps, Process.get(:hit)}
    end
  end

  ## Optimizations

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

  ## Joining of two same calls

  ### zip + reduce to zip_reduce
  def join_consecutive([{_, :zip, []}, {Enum, :reduce, [acc, reducer]} | tail]) do
    hit()
    [{Enum, :zip_reduce, [acc, reducer]} | join_consecutive(tail)]
  end

  ### Pairs of mappers, filters etc.
  def join_consecutive([{left_eos, left_op, [left]}, {right_eos, right_op, [right]} | tail]) when is_eos(left_eos) and is_eos(right_eos) do
    if left_eos == Stream or is_pure_fn(left) or is_pure_fn(right) do
      case {left_op, right_op} do
        # Same ops
        {:filter, :filter} ->
          hit()
          body = evaluate tri fn x -> left.(x) && right.(x) end
          [{right_eos, :filter, [body]} | join_consecutive(tail)]

        {:map, right_op} when right_op in ~w[map flat_map each]a ->
          hit()
          body = evaluate tri fn x -> right.(left.(x)) end
          [{right_eos, right_op, [body]} | join_consecutive(tail)]

        {:map, :filter} ->
          hit()
          body = evaluate tri fn x ->
            value = left.(x)
            if right.(value), do: [value], else: []
          end
          [{right_eos, :flat_map, [body]} | join_consecutive(tail)]

        {:flat_map, :flat_map} ->
          hit()
          body = evaluate tri fn x -> Enum.flat_map(left.(x), right) end
          [{right_eos, :flat_map, [body]} | join_consecutive(tail)]

        {:drop, :drop} ->
          hit()
          # I don't think this optimization hits at least once
          body = evaluate tri fn x -> Enum.drop(x, left + right) end
          [{right_eos, :drop, [body]} | join_consecutive(tail)]

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
    if eos == Stream or (is_pure(accumulator) and (is_pure_fn(func) or is_pure_fn(reducer))) do
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
          body = evaluate tri fn x, acc -> Enum.reduce(func.(x), acc, reducer) end
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

  ### with Map.new
  def join_consecutive([{eos, :map, [left]} = first, {Map, :new, [right]} = second | tail]) when is_eos(eos) do
    if eos == Stream or is_pure_fn(left) do
      hit()
      func = evaluate tri fn x -> right.(left.(x)) end
      [{Map, :new, [func]} | join_consecutive(tail)]
    else
      [first | join_consecutive([second | tail])]
    end
  end

  def join_consecutive([{eos, :map, [left]}, {Map, :new, []} | tail]) when is_eos(eos) do
    hit()
    func = evaluate tri fn x -> left.(x) end
    [{Map, :new, [func]} | join_consecutive(tail)]
  end

  ### Other
  def join_consecutive([head | tail]), do: [head | join_consecutive(tail)]
  def join_consecutive([]), do: []

  ## Enum.into(_, %{}) to Map.new

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

  ## Loop unrolling

  # #TODO
  # def unroll(list, [{Enum, :map, [func]} | tail]) when is_list(list) do

  # end
  # def unroll(argument, steps), do: {argument, steps}

  ## Chaining and unchaining

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

  ## Evalution and Analysis helper

  defp evaluate(ast) do
    ast
    |> SSATranslator.from_tria!()
    |> Evaluation.run_while(remove_unused: true)
  end

  defp is_pure_fn({:fn, _, [clauses]}) do
    is_pure(clauses)
  end
  defp is_pure_fn(_), do: false

end
