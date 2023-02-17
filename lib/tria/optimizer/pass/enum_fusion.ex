defmodule Tria.Optimizer.Pass.EnumFusion do

  @moduledoc """
  Joining, optimizing and other stuff for Enum

  #TODO translate enum functions to `:lists` where applicable
  #TODO unrolling
  """

  # `eos` in this module means `enum_or_stream`

  use Tria.Optimizer.Pass

  import Tria.Language
  import Tria.Language.Analyzer, only: [is_pure: 1, is_safe: 1]
  import Tria.Language.Tri
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Optimizer.Depmap
  alias Tria.Optimizer.Pass
  alias Tria.Optimizer.Pass.Evaluation

  defstruct [
    hit: false,
    depmap: %Depmap{}
  ]

  @type state :: %__MODULE__{
    hit: boolean(),
    depmap: Depmap.t()
  }

  @typedoc """
  Options for this pass

  - `:state` -- initial state of the optimization
  - `:return_depmap` -- whether to return depmap along the optimization
  """
  @type option :: {:state, state()}
  | {:return_depmap, boolean()}

  #@common_ops ~w[filter reject map reduce sum count product frequencies flat_map foldl foldr]a
  #@list_returning_op ~w[filter reject map concat]a

  defguard is_fusable(module, function, arity)
           when module == Enum or module == Stream
           or (module == Map and function == :new)
           or (module == :lists and function == :reverse and arity == 1)

  defguard is_eos(eos) when eos in [Enum, Stream]

  @tri_opts meta: false

  defmacrop hit(state) do
    line = __CALLER__.line
    quote do
      state = unquote(state)
      Tracer.tag(unquote(line), label: :fusion_hit_at)
      %__MODULE__{state | hit: true}
    end
  end

  defp item >>> {steps, state} do
    {[item | steps], state}
  end

  ## Pass callbacks

  def begin(ast, opts) do
    state = Keyword.get(opts, :state, %__MODULE__{})
    {argument, steps} = unchain(ast)
    {:ok, {argument, steps, state}}
  end

  def run_once({argument, steps, state}, opts) do
    case run_pipeline(steps, state, opts) do
      {steps, %__MODULE__{hit: true}} ->
        {:ok, {argument, steps, state}}

      {_, _} ->
        {:error, :nothing_to_optimize}
    end
  end

  def finish({argument, steps, %__MODULE__{depmap: depmap}}, opts) do
    ast = chain(argument, steps)
    if opts[:return_depmap] do
      {:ok, {ast, depmap}}
    else
      {:ok, ast}
    end
  end

  def run_while(ast, opts \\ []) do
    if opts[:return_depmap] do
      postwalk(ast, %Depmap{}, fn
        dot_call(module, function, args) = ast, depmap when is_fusable(module, function, length(args)) ->
          case Pass.run_until(__MODULE__, ast, opts) do
            {:ok, {result, new_depmap}} ->
              {result, Depmap.merge(depmap, new_depmap)}

            _ ->
              {ast, depmap}
          end

        other, depmap ->
          {other, depmap}
      end)
    else
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
  end

  defp run_pipeline(steps, state, _opts) do
    {steps, state} = reducify(steps, state)
    {steps, state} = unreject(steps, state)
    {steps, state} = unstream(steps, state)
    {steps, state} = join_consecutive(steps, state)
    {steps, state} = into_to_map_new(steps, state)

    {steps, state}
  end

  ## Optimizations

  ### Converts `reject` to `filter`

  def unreject([{eos, :reject, [the_fn]} | tail], state) do
    state = hit(state)
    {body, state} = evaluate(tri(fn x -> ! the_fn.(x) end), state)
    {eos, :filter, [body]} >>> unreject(tail, state)
  end
  def unreject([head | tail], state) do
    head >>> unreject(tail, state)
  end
  def unreject([], state), do: {[], state}

  ### Removing Stream before Enum

  def unstream([{Stream, op, args}, {Enum, :to_list, []} | tail], state) when op in ~w[map filter]a do
    {Enum, op, args} >>> unstream(tail, hit state)
  end
  def unstream([head | tail], state), do: head >>> unstream(tail, state)
  def unstream([], state), do: {[], state}

  ### Converts some operations to reduce

  def reducify([step | tail], state) do
    case step do
      {eos, :sum, []} ->
        body = tri fn x, acc -> x + acc end
        {eos, :reduce, [0, body]} >>> reducify(tail, hit state)

      {eos, :count, []} ->
        body = tri fn _, acc -> acc + 1 end
        {eos, :reduce, [0, body]} >>> reducify(tail, hit state)

      {eos, :product, []} ->
        body = tri fn x, acc -> x * acc end
        {eos, :reduce, [1, body]} >>> reducify(tail, hit state)

      {eos, :frequencies, []} ->
        body = tri fn x, acc -> Map.update(acc, x, 1, fn y -> y + 1 end) end
        {eos, :reduce, [tri(%{}), body]} >>> reducify(tail, hit state)

      _ ->
        step >>> reducify(tail, state)
    end
  end
  def reducify([], state), do: {[], state}

  ## Joining of two same calls

  ### zip + reduce to zip_reduce
  def join_consecutive([{_, :zip, []}, {Enum, :reduce, [acc, reducer]} | tail], state) do
    {Enum, :zip_reduce, [acc, reducer]} >>> join_consecutive(tail, hit state)
  end

  ### Pairs of mappers, filters etc.
  def join_consecutive([{left_eos, left_op, [left]}, {right_eos, right_op, [right]} | tail], state) when is_eos(left_eos) and is_eos(right_eos) do
    if left_eos == Stream or is_fusable_fn(left) or is_fusable_fn(right) do
      case {left_op, right_op} do
        # Same ops
        {:filter, :filter} ->
          state = put_pure_fns(state, [left, right])
          {body, state} = evaluate(tri(fn x -> left.(x) && right.(x) end), state)
          {right_eos, :filter, [body]} >>> join_consecutive(tail, hit state)

        {:map, right_op} when right_op in ~w[map flat_map each]a ->
          state = put_pure_fns(state, [left, right])
          {body, state} = evaluate(tri(fn x -> right.(left.(x)) end), state)
          {right_eos, right_op, [body]} >>> join_consecutive(tail, hit state)

        {:map, :filter} ->
          state = put_pure_fns(state, [left, right])
          {body, state} =
            tri do
              fn x ->
                value = left.(x)
                if right.(value), do: [value], else: []
              end
            end
            |> evaluate(state)

          {right_eos, :flat_map, [body]} >>> join_consecutive(tail, hit state)

        {:flat_map, :flat_map} ->
          state = put_pure_fns(state, [left, right])
          {body, state} = evaluate(tri(fn x -> Enum.flat_map(left.(x), right) end), state)
          {right_eos, :flat_map, [body]} >>> join_consecutive(tail, hit state)

        {:drop, :drop} ->
          ### I don't think this optimization hits at least once
          state = put_pure_fns(state, [left, right])
          {body, state} = evaluate(tri(fn x -> Enum.drop(x, left + right) end), state)
          {right_eos, :drop, [body]} >>> join_consecutive(tail, hit state)

        {:filter, :each} ->
          state = put_pure_fns(state, [left, right])
          {body, state} = evaluate(tri(fn x -> if left.(x), do: right.(x) end), state)
          {right_eos, :each, [body]} >>> join_consecutive(tail, hit state)

        _ ->
          {left_eos, left_op, [left]} >>> join_consecutive([{right_eos, right_op, [right]} | tail], state)
      end
    else
      {left_eos, left_op, [left]} >>> join_consecutive([{right_eos, right_op, [right]} | tail], state)
    end
  end

  ### Joining with reduce
  def join_consecutive([{eos, op, [func]}, {Enum, :reduce, [accumulator, reducer]} = step | tail], state) do
    if eos == Stream or (is_pure(accumulator) and (is_fusable_fn(func) or is_fusable_fn(reducer))) do
      case op do
        :map ->
          state = put_pure_fns(state, [accumulator, func, reducer])
          {body, state} = evaluate(tri(fn x, acc -> reducer.(func.(x), acc) end), state)
          {Enum, :reduce, [accumulator, body]} >>> join_consecutive(tail, hit state)

        :filter ->
          state = put_pure_fns(state, [accumulator, func, reducer])
          {body, state} = evaluate(tri(fn x, acc -> if func.(x), do: reducer.(x, acc), else: acc end), state)
          {Enum, :reduce, [accumulator, body]} >>> join_consecutive(tail, hit state)

        :flat_map ->
          state = put_pure_fns(state, [accumulator, func, reducer])
          {body, state} = evaluate(tri(fn x, acc -> Enum.reduce(func.(x), acc, reducer) end), state)
          {Enum, :reduce, [accumulator, body]} >>> join_consecutive(tail, hit state)

        _ ->
          {eos, op, [func]} >>> join_consecutive([step | tail], state)
      end
    else
      {eos, op, [func]} >>> join_consecutive([step | tail], state)
    end
  end
  def join_consecutive([{_eos, :concat, []}, {Enum, :reduce, [accumulator, reducer]} | tail], state) do
    {body, state} = evaluate(tri(fn x, acc -> Enum.reduce(x, acc, reducer) end), state)
    {Enum, :reduce, [accumulator, body]} >>> join_consecutive(tail, hit state)
  end

  ### Elixir's `for` optimization
  def join_consecutive([{Enum, :reduce, [[], reducer]} = reduce, {:lists, :reverse, []} = reverse | tail], state) do
    case reducer do
      tri(fn x, acc -> [body | acc] end) ->
        {Enum, :map, [tri(fn x -> body end)]} >>> join_consecutive(tail, hit state)

      _ ->
        reduce >>> join_consecutive([reverse | tail], state)
    end
  end

  ### with Map.new
  def join_consecutive([{eos, :map, [left]} = first, {Map, :new, [right]} = second | tail], state) when is_eos(eos) do
    if eos == Stream or is_fusable_fn(left) do
      state = put_pure_fn(state, left)
      {func, state} = evaluate(tri(fn x -> right.(left.(x)) end), state)
      {Map, :new, [func]} >>> join_consecutive(tail, hit state)
    else
      first >>> join_consecutive([second | tail], state)
    end
  end

  def join_consecutive([{eos, :map, [left]}, {Map, :new, []} | tail], state) when is_eos(eos) do
    {func, state} = evaluate(tri(fn x -> left.(x) end), state)
    {Map, :new, [func]} >>> join_consecutive(tail, hit state)
  end

  ### Other
  def join_consecutive([head | tail], state), do: head >>> join_consecutive(tail, state)
  def join_consecutive([], state), do: {[], state}

  ## Enum.into(_, %{}) to Map.new

  def into_to_map_new([{Enum, :into, [tri(%{}), func]} | tail], state) do
    {Map, :new, [func]} >>> into_to_map_new(tail, hit state)
  end
  def into_to_map_new([{Enum, :into, [tri(%{})]} | tail], state) do
    {Map, :new, []} >>> into_to_map_new(tail, hit state)
  end
  def into_to_map_new([head | tail], state), do: head >>> into_to_map_new(tail, state)
  def into_to_map_new([], state), do: {[], state}

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

  defp is_fusable_fn({:fn, _, [clauses]}) do
    is_pure(clauses) and is_safe(clauses)
  end
  defp is_fusable_fn(_), do: false

  defp evaluate(ast, %__MODULE__{depmap: depmap} = state) do
    {ast, evaluation_depmap} =
      ast
      |> SSATranslator.from_tria!()
      |> Evaluation.run_while(remove_unused: true, return_depmap: true)

    {ast, %{state | depmap: Depmap.merge(depmap, evaluation_depmap)}}
  end

  defp put_pure_fn(%__MODULE__{depmap: depmap} = state, {:fn, _, [clauses]}) do
    %{state | depmap: Depmap.put(depmap, :pure, clauses)}
  end
  defp put_pure_fn(state, _), do: state

  defp put_pure_fns(state, fns) do
    Enum.reduce(fns, state, &put_pure_fn(&2, &1))
  end

end
