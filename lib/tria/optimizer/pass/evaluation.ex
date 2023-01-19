defmodule Tria.Optimizer.Pass.Evaluation do

  @moduledoc """
  This pass tries to evaluate everything what
  can be preevaluated
  """

  use Tria.Optimizer.Pass
  import Tria.Language
  import Tria.Language.Analyzer, only: [is_pure: 1, is_safe: 1]
  import Tria.Language.Binary, only: [traverse_specifier: 3]
  import Tria.Language.Guard
  import Tria.Language.Meta
  import Tria.Language.Tri
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Language.Analyzer.Purity
  alias Tria.Language.Bindmap
  alias Tria.Language.Interpreter
  alias Tria.Language.MFArity

  defstruct [
    # Runtime variable which is passed during traversal.
    # Contains a context
    bindings: %{},

    # Results of the evaluation are stored here
    hit: false,
    used: %{},
    evaluated: MapSet.new(),
  ]

  @type state :: %__MODULE__{
    bindings: Bindmap.t(),
    hit: boolean(),
    used: map(),
    evaluated: MapSet.t()
  }

  @typedoc """
  Option for run_* family of functions

  `:remove_unused` -- whether to remove unused variables and inline used-once or not (default: `true`)
  `:inspect_iteration` -- only for `run_while`, whether to inspect ast on every iteration (default: `false`).
  Useful for debugging, accepts boolean or label atom
  `:state` -- allows to define state of evaluation
  """
  @type option :: {:remove_unused, boolean()}
  | {:inspect_iteration, atom()}
  | {:state, state()}

  ## Pass callbacks

  def begin(ast, _opts) do
    {:ok, ast}
  end

  def finish(ast, _opts) do
    {:ok, ast}
  end

  ## Public API

  @spec run_once(Tria.t(), [option()]) :: {:ok, Tria.t()} | {:error, :nothing_to_optimize}
  def run_once(ssa_ast, opts \\ []) do
    Tracer.tag_ast(ssa_ast, label: :evaluation_run_once)

    state = opts_to_state(opts)
    {result, state} = run(ssa_ast, state)
    {result, state} =
      if Keyword.get(opts, :remove_unused, true) do
        result
        |> run_remove_unused(state)
        |> tap(fn {ast, _} -> Tracer.tag_ast(ast, label: :evaluation_unused_removed) end)
      else
        {result, state}
      end

    case state do
      %{hit: false} -> {:error, :nothing_to_optimize}
      _ -> {:ok, result}
    end
  end

  @spec run_once!(Tria.t(), [option()]) :: Tria.t()
  def run_once!(ssa_ast, opts \\ []) do
    {result, _state} = run(ssa_ast, Keyword.get(opts, :state, %__MODULE__{}))
    result
  end

  defp opts_to_state(state: %__MODULE__{} = state), do: state
  defp opts_to_state(_opts), do: %__MODULE__{}

  ## Runner helper

  # Since this is a private macro, it is defined above usage
  defmacrop hit(state) do
    quote do
      Tracer.tag(unquote(__CALLER__.line), label: :hit_at)
      # IO.inspect(unquote(__CALLER__.line), label: :hit_at)
      %{unquote(state) | hit: true}
    end
  end

  ## Runners

  @doc """
  Evaluation runner for general normal context
  """
  def run(ast, state) do
    case ast do
      # Equals
      {:"=", meta, [left, right]} ->
        {left, left_state} = run_match(left, state)
        {right, right_state} = run(right, state)
        { block, state }  =
          case Interpreter.match(left, right) do
            {_yes_or_maybe, [{new_left, new_right}]} ->
              state = if equal?(left, new_left), do: state, else: hit(state)
              { {:=, meta, [new_left, new_right]}, put_bind(state, new_left, new_right) }

            {_yes_or_maybe, binds} ->
              state = hit Enum.reduce(binds, state, fn {key, value}, state -> put_bind(state, key, value) end)
              { {:=, meta, [left, right]}, state }

            :no ->
              q = quote do: raise MatchError, term: unquote right
              { q, hit state }
          end

        { block, right_state <~ (left_state <~ state) }

      # Block
      {:__block__, _, []} = empty_block ->
        { empty_block, state }

      {:__block__, _, [line]} ->
        run(line, hit state)

      {:__block__, meta, lines} ->
        {lines, state} =
          Enum.flat_map_reduce(lines, state, fn line, state ->
            {line, state} = run(line, state)
            case line do
              {:__block__, _, lines} -> { lines, hit state }
              other -> {[other], state}
            end
          end)

        {last_line, lines} = List.pop_at(lines, -1)
        {lines, state} =
          Enum.flat_map_reduce(lines, state, fn line, state ->
            cond do
              match?({:=, _, _}, line) ->
                {[line], state}

              is_fn(line) or vared_literal?(line) ->
                {[], hit state}

              is_pure(line) and is_safe(line) ->
                # Binds must be in the correct order
                {_, binds} =
                  prewalk(line, [], fn
                    tri(_key = value) = match, acc when is_fn(value) ->
                      {nil, [match | acc]}

                    tri(_key = _value) = match, acc ->
                      {match, [match | acc]}

                    the_fn, acc when is_fn(the_fn) ->
                      {nil, acc}

                    other, acc ->
                      {other, acc}
                  end)

                {:lists.reverse(binds), hit state}

              true ->
                {[line], state}
            end
          end)

        { {:__block__, meta, lines ++ [last_line]}, state }

      # Try
      {:try, meta, [[{:do, body} | other]]} = the_try ->
        safe_and_pure = is_pure(body) and is_safe(body)
        cond do
          safe_and_pure and (other[:else] in [nil, []]) ->
            { body, hit state }

          safe_and_pure ->
            run({:case, meta, [body, [do: other[:else]]]}, hit state)

          true ->
            run_try(the_try, state)
        end

      # Receive
      {:receive, meta, [[do: clauses]]} ->
        {clauses, clauses_state} = run_clauses(clauses, state)
        { {:receive, meta, [[do: clauses]]}, state <~> clauses_state }

      {:receive, meta, [[do: clauses, after: {timeout, body}]]} ->
        {clauses, clauses_state} = run_clauses(clauses, state)
        {timeout, timeout_state} = run(timeout, state)
        {body, body_state} = run(body, timeout_state)
        state = state <~> clauses_state <~> body_state
        { {:receive, meta, [[do: clauses, after: {timeout, body}]]}, state }

      # Case
      {:case, meta, [arg, [do: clauses]]} ->
        {arg, state} = run(arg, state)
        case run_clauses([arg], clauses, state, fn [arg] -> tri(raise CaseClauseError, term: arg) end) do
          {:block, block, block_state} ->
            { block, state <~> block_state}

          {:clauses, clauses, clauses_state} ->
            { {:case, meta, [arg, [do: clauses]]}, state <~> clauses_state}
        end

      # Fn Inlining
      {{:".", _dotmeta, [{:fn, _, clauses}]}, _meta, args} ->
        state = hit(state)
        {args, args_state} = run(args, state)

        case run_clauses(args, clauses, state, fn args -> tri(raise FunctionClauseError, args: args) end) do
          {:block, block, block_state} ->
            { block, state <~ args_state <~> block_state }

          {:clauses, clauses, clauses_state} ->
            clauses =
              Enum.map(clauses, fn {:"->", meta, [pattern, body]} ->
                pattern =
                  case pattern do
                    [{:when, whenmeta, [args, guard]}] ->
                      [{:when, whenmeta, [tuplify(List.wrap args), guard]}]

                    pattern ->
                      [tuplify(pattern)]
                  end

                {:->, meta, [pattern, body]}
              end)

            body = [tuplify(args), [do: clauses]]

            { {:case, [], body}, state <~ args_state <~> clauses_state }
        end

      # Fn
      {:fn, meta, clauses} ->
        {clauses, clauses_state} = run_clauses(clauses, state)
        { {:fn, meta, clauses}, state <~> clauses_state }

      # Binary
      {:<<>>, meta, parts} ->
        {parts, state} =
          Enum.map_reduce(parts, state, fn
            {:"::", meta, [value, ts]}, new_state ->
              {ts, ts_state} =
                traverse_specifier(ts, new_state, fn ts_input, new_state ->
                  {ts_input, state} = run(ts_input, state)
                  { ts_input, new_state <~> state }
                end)

              {value, value_state} = run(value, state)
              { {:"::", meta, [value, ts]}, (new_state <~> ts_state) <~ value_state }

            other, new_state ->
              {other, other_state} = run(other, state)
              { other, new_state <~ other_state }
          end)

        { {:<<>>, meta, parts}, state }

      # List cons
      [{:|, meta, [left, right]}] ->
        {[left, right], state} = run([left, right], state)
        cond do
          is_list right ->
            { [left | right], hit state }

          is_list unfolded = unfold(right, state) ->
            { [left | unfolded ], hit state }

          true ->
            { [{:|, meta, [left, right]}], state }
        end

      # List
      [head | tail] ->
        {head, head_state} = run(head, state)
        {tail, tail_state} = run(tail, state)
        { [head | tail], head_state <~ tail_state }

      # Twople
      {left, right} ->
        {[left, right], state} = run([left, right], state)
        { {left, right}, state }

      # Tuple
      {:{}, meta, items} ->
        {items, state} = run(items, state)
        { {:{}, meta, items}, state }

      # Map cons
      {:%{}, meta, [{:|, consmeta, [map, pairs]}]} ->
        {map, map_state} = run(map, state)
        {pairs, pairs_state} = run(pairs, state)
        state = map_state <~ pairs_state
        case map do
          {:%{}, _, [{:|, _, [map, map_pairs]}]} ->
            { {:%{}, meta, [{:|, consmeta, [map, map_pairs ++ pairs]}]}, hit state }

          {:%{}, _, map_pairs} ->
            { {:%{}, meta, map_pairs ++ pairs}, hit state }

          map ->
            { {:%{}, meta, [{:|, consmeta, [map, pairs]}]}, state }
        end

      # Map
      {:%{}, meta, pairs} ->
        {pairs, state} = run(pairs, state)
        { {:%{}, meta, pairs}, state }

      # Variable
      variable when is_variable(variable) ->
        state = put_used(state, variable)
        case fetch_bind(state, variable) do
          :error ->
            { variable, state }

          {:ok, new_variable} when is_variable(new_variable) ->
            { new_variable, hit state }

          {:ok, the_fn} when is_fn(the_fn) ->
            # Since we propagate the `fn`, we'll need to retranslate it
            # To preserve the static assignment form
            # And we rerun to have the right bindmap
            {the_fn, state} =
              the_fn
              |> SSATranslator.from_tria!()
              |> run(state)

            { the_fn, hit state }

          {:ok, other} ->
            cond do
              quoted_literal?(other) ->
                { other, hit state }

              true ->
                { variable, state }
            end
        end

      # Module.function(args)
      dot_call(module, function, args, dotmeta, meta) when is_atom(module) and is_atom(function) and is_list(args) ->
        {args, state} = run(args, state)
        call = dot_call(module, function, args, dotmeta, meta)
        precompute(call, state)

      # Other calls
      {call, meta, args} ->
        {[call, args], state} = run([call, args], state)
        { {call, meta, args}, state }

      # Literals
      other ->
        if quoted_literal?(other) do
          { other, state }
        else
          IO.inspect(other, label: :ast)
          inspect_ast(other, label: :code)
          raise "Unexpected element"
        end
    end
  end

  @doc """
  Runner for clauses which generates
  """
  def run_clauses(args, clauses, state, error_fn) do
    original_length = length clauses
    unfolded_args = Enum.map(args, fn arg -> unfold(arg, state) end)

    clauses
    |> Enum.map(fn {:"->", meta, [left, right]} ->
      {left, state} = run_match(left, state)
      {Interpreter.multimatch(left, unfolded_args), {left, state, meta, right}}
    end)
    |> filter_clauses()
    |> case do
      [{{:yes, bindings}, {_pattern, state, meta, body}} | _] ->
        lines =
          bindings
          |> Enum.map(fn {key, value} -> {:=, meta, [key, value]} end)
          |> Kernel.++([body])

        block = {:__block__, meta, lines}
        {block, state} = run(block, state)
        {:block, block, hit(state)}

      [] ->
        {:block, error_fn.(args), hit(state)}

      binds_and_clauses ->
        state = if length(binds_and_clauses) == original_length, do: state, else: hit(state)

        {clauses, state} =
          Enum.map_reduce(binds_and_clauses, state, fn {{_, bindings}, {left, left_state, meta, body}}, acc_state ->
            state = put_binds(state, bindings)
            {right, state} = run(body, state <~ left_state)

            case left do
              [{:when, _, _} = pattern] ->
                { {:->, meta, [[pattern], right]}, acc_state <~> state }

              pattern ->
                { {:->, meta, [pattern, right]}, acc_state <~> state }
            end
          end)

        {:clauses, clauses, state}
    end
  rescue
    e ->
      {:case, [], [args, [do: clauses]]}
      |> inspect_ast(label: :failed)
      reraise e, __STACKTRACE__
  end

  def run_guard(ast, state) do
    new_ast =
      Bindmap.unfold_while(ast, state.bindings, fn ast ->
        if is_guard(ast), do: :cont, else: :skip
      end)

    {_, variables} =
      postwalk(ast, [], fn
        variable, vars when is_variable(variable) -> {variable, [variable | vars]}
        other, vars -> {other, vars}
      end)

    state = Enum.reduce(variables, state, &put_used(&2, &1))
    state = if new_ast != ast, do: hit(state), else: state

    { new_ast, state }
  end

  def run_match(ast, state, new_state \\ %__MODULE__{}) do
    case ast do
      # Binary matching is special because pinning is not required for variables in types
      {:"<<>>", meta, items} ->
        {items, new_state} =
          Enum.map_reduce(items, new_state, fn
            {:"::", meta, [item, ts]}, new_state ->
              # First we just put variables into the ts part of the binary match
              merged_state = state <~ new_state
              {ts, ts_state} =
                traverse_specifier(ts, merged_state, fn ts_input, merged_state ->
                  {ts_input, runned_state} = run(ts_input, merged_state)
                  {ts_input, merged_state <~> runned_state}
                end)

              # And then we get definitions from left side of ::
              {item, new_state} = run_match(item, state, new_state)

              {{:"::", meta, [item, ts]}, new_state <~> ts_state}

            item, new_state ->
              run_match(item, state, new_state)
          end)

        { {:"<<>>", meta, items}, new_state }

      # When
      {:when, meta, pattern_and_guard} ->
        {guard, patterns} = List.pop_at(pattern_and_guard, -1)
        {patterns, pattern_state} = run_match(patterns, state, new_state)

        # Since guard is not creating new variables, we can just run evaluation against it
        {guard, guard_state} = run_guard(guard, state <~ pattern_state)

        { {:when, meta, patterns ++ [guard]}, pattern_state <~> guard_state }

      # Pin . We handle pin separately before variables
      pin(variable, meta) = pinned when is_variable(variable) ->
        case fetch_bind(state, variable) do
          :error ->
            new_state = put_used(state, variable)
            { pinned, new_state }

          {:ok, val} when is_variable(val) ->
            { pin(val, meta), new_state }

          {:ok, val} ->
            cond do
              quoted_literal?(val) ->
                { val, new_state }

              vared_literal?(val) ->
                { pin_variables(val), new_state }

              true ->
                new_state = put_used(state, variable)
                { pinned, new_state }
            end
        end

      # Variable
      variable when is_variable(variable)  ->
        { variable, new_state }

      # Map or Tuple or Binary or matchable operators (like `<>` or `|`)
      {n, m, items} when is_list(items) ->
        {items, new_state} = run_match(items, state, new_state)
        { {n, m, items}, new_state }

      # List
      items when is_list(items) ->
        Enum.map_reduce(items, new_state, fn item, acc_state ->
          run_match(item, state, acc_state)
        end)

      # Twople
      {left, right} ->
        {left, new_state} = run_match(left, state, new_state)
        {right, new_state} = run_match(right, state, new_state)

        { {left, right}, new_state }

      # Literal
      literal when is_literal(literal) ->
        { literal, new_state }
    end
  end

  def run_try({:try, meta, [clauses]}, state) do
    {clauses, state} =
      Enum.map_reduce(clauses, state, fn
        {do_or_after, body}, acc_state when do_or_after in ~w[do after]a ->
          {body, body_state} = run(body, state)
          {{do_or_after, body}, acc_state <~> body_state}

        {catch_or_else, clauses}, acc_state when catch_or_else in ~w[catch else]a ->
          {clauses, acc_state} = run_clauses(clauses, acc_state)
          {{catch_or_else, clauses}, acc_state}
      end)

    {{:try, meta, [clauses]}, state}
  end

  def run_clauses(clauses, state) do
    Enum.map_reduce(clauses, state, fn {:"->", meta, [left, right]}, clauses_state ->
      {left, left_state} = run_match(left, state)
      {right, right_state} = run(right, state <~ left_state)

      { {:"->", meta, [left, right]}, clauses_state <~> right_state }
    end)
  end

  ## Removing unused

  def run_remove_unused(ast, state) do
    #TODO improve
    context_prewalk(
      ast,
      state,
      fn
        tri(left = right) = ast, new_state, nil when is_variable(left) ->
          left = unmeta left
          case count_used(state, left) do
            0 -> {right, hit new_state}
            _ -> {ast, new_state}
          end

        variable, new_state, nil when is_variable(variable) ->
          variable = unmeta variable
          with(
            1 <- count_used(state, variable),
            {:ok, value} <- fetch_bind(state, variable),
            true <- is_fn(value) or vared_literal?(value) or (is_pure(value) and is_safe(value))
          ) do
            value = SSATranslator.from_tria!(value)
            { value, hit delete_used(new_state, variable) }
          else
            _ -> { variable, new_state }
          end

        other, new_state, _ ->
          {other, new_state}
      end,
      nil
    )
  end

  ## Helpers

  defp filter_clauses([{{:yes, _}, _} = last | _tail]) do
    [last]
  end
  defp filter_clauses([{:no, _} | tail]) do
    filter_clauses(tail)
  end
  defp filter_clauses([other | tail]), do: [other | filter_clauses(tail)]
  defp filter_clauses([]), do: []

  defp equal?(left, right), do: unmeta(left) == unmeta(right)

  defp precompute(dot_call(module, function, args) = dc, state) when is_atom(module) and is_atom(function) do
    args = Enum.map(args, fn arg -> unfold(arg, state) end)
    with(
      true <- Enum.all?(args, &quoted_literal?/1),
      {:pure, {result, bindings}, _} <- Purity.run_analyze(dc)
    ) do
      Tracer.tag_ast(dot_call(module, function, args), label: :precomputed_from)
      Tracer.tag(result, label: :precomputed_to)
      try do
        Macro.escape(result)
      rescue
        _ -> {dc, state}
      else
        result ->
          case bindings do
            [] ->
              state = put_evaluated(state, dc)
              {result, hit(state)}

            bindings ->
              state = put_evaluated(state, dc)
              block = {:__block__, [], Enum.map(bindings, fn {key, value} -> tri(key = value) end) ++ [result]}
              {block, hit(state)}
          end
      end
    else
      _ -> {dc, state}
    end
  end

  #TODO rename to something more meaningful
  defp tuplify([arg]), do: arg
  defp tuplify([left, right]), do: {left, right}
  defp tuplify(args) when is_list(args), do: {:{}, [], args}

  defp pin_variables(ast) do
    postwalk(ast, fn
      variable when is_variable variable -> pin variable
      other -> other
    end)
  end

  ## State helpers

  # defp get_bind(%{bindings: bindings}, key, default \\ nil) do
  #   Map.get(bindings, unmeta(key), default)
  # end

  # defp has_used?(%{used: used}, key) do
  #   :erlang.is_map_key(unmeta(key), used)
  # end

  defp fetch_bind(%{bindings: bindings}, key) do
    Bindmap.fetch_unfolded(bindings, key)
  end

  defp put_binds(%{bindings: bindings} = state, pairs) do
    pairs = Enum.reject(pairs, fn {key, _value} -> quoted_literal?(key) end)
    %{state | bindings: Bindmap.put(bindings, pairs)}
  end

  defp put_bind(%{bindings: bindings} = state, key, value) do
    key = unmeta key
    # {value, _} = run_remove_unused(value, %__MODULE__{state | used: %{}})
    # value = run_while(value, state: state, remove_unused: false)

    if quoted_literal? key do
      state
    else
      %{state | bindings: Bindmap.put(bindings, key, value)}
    end
  end

  defp put_evaluated(%{evaluated: evaluated} = state, call) do
    %{state | evaluated: MapSet.put(evaluated, MFArity.to_mfarity(call))}
  end

  defp put_used(%{used: used} = state, key) do
    uid = :erlang.unique_integer([:positive])
    key = unmeta(key)
    used =
      case used do
        %{^key => mapset} -> %{used | key => MapSet.put(mapset, uid)}
        _ -> Map.put(used, key, MapSet.new([uid]))
      end

    %{state | used: used}
  end

  defp delete_used(%{used: used} = state, key) do
    %{state | used: Map.delete(used, unmeta key)}
  end

  defp count_used(%{used: used}, key) do
    key = unmeta key
    case used do
      %{^key => mapset} -> MapSet.size(mapset)
      _ -> 0
    end
  end

  defp unfold(value, %{bindings: bindings}) do
    Bindmap.unfold_while(value, bindings, fn ast ->
      if vared_literal?(ast), do: :cont, else: :halt
    end)
  end

  defp left <~ right do
    left <~> right
  end

  defp left <~> right do
    %{hit: left_hit, bindings: left_bindings, used: left_used}  = left
    %{hit: right_hit, bindings: right_bindings, used: right_used} = right

    used = Map.merge(left_used, right_used, fn _, l, r -> MapSet.union(l, r) end)
    bindings = Bindmap.merge!(left_bindings, right_bindings)

    %{left | used: used, hit: left_hit or right_hit, bindings: bindings}
  end

end
