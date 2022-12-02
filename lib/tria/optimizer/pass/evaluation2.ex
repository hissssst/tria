defmodule Tria.Optimizer.Pass.Evaluation2 do

  @moduledoc """
  This pass tries to evaluate everything what
  can be preevaluated
  """

  import Tria.Debug.Breakpoint
  import Tria.Language
  import Tria.Language.Binary, only: [traverse_specifier: 3]
  import Tria.Language.Guard
  import Tria.Language.Tri
  import Tria.Language.Meta
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Language.Analyzer.Purity, warn: false
  alias Tria.Language.Bindmap
  alias Tria.Language.Interpreter

  defstruct [
    bindings: %{},
    hit: false,
    used: %{},
  ]

  ## Public API

  def run_once(ssa_ast, _opts \\ []) do
    {result, state} = run(ssa_ast, %__MODULE__{})
    case state do
      %{hit: false} -> {:error, :nothing_to_evaluate}
      _ -> {:ok, result}
    end
  end

  def run_once!(ssa_ast, _opts \\ []) do
    {result, _state} = run(ssa_ast, %__MODULE__{})
    result
  end

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
      # Breakpoint
      breakpoint(point) ->
        handle_breakpoint(point)

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
              block = {:__block__, [], Enum.map(binds, fn {l, r} -> {:=, meta, [l, r]} end) ++ [right]}
              { block, hit put_bind(state, left, right) }

            :no ->
              q = quote do: raise MatchError, term: unquote right
              { q, hit state }
          end

        { block, right_state <~ left_state <~ state }

      # Block
      {:__block__, _, []} = empty_block ->
        { empty_block, state }

      {:__block__, meta, lines} ->
        {lines, state} =
          Enum.flat_map_reduce(lines, state, fn line, state ->
            {line, state} = run(line, state)
            case line do
              {:__block__, _, lines} -> { lines, hit(state) }
              other -> {[other], state}
            end
          end)

        {last_line, lines} = List.pop_at(lines, -1)
        {lines, state} =
          Enum.flat_map_reduce(lines, state, fn line, state ->
            if Macro.quoted_literal?(line), do: {[], hit state}, else: {[line], state}
          end)

        { {:__block__, meta, lines ++ [last_line]}, state }

      # Try
      {:try, [], [[{:do, body} | _other]]} = the_try ->
        if Purity.check_analyze(body) do
          { body, hit state }
        else
          run_try(the_try, state)
        end

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
                      [{:when, whenmeta, [tuplify(args), guard]}]

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
        {clauses, clauses_state} =
          Enum.map_reduce(clauses, state, fn {:"->", meta, [left, right]}, clauses_state ->
            {left, left_state} = run_match(left, state)
            {right, right_state} = run(right, state <~ left_state)

            { {:"->", meta, [left, right]}, clauses_state <~> right_state }
          end)

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
            the_fn = SSATranslator.from_tria(the_fn)
            { the_fn, hit state }

          {:ok, other} ->
            cond do
              Macro.quoted_literal?(other) ->
                { other, hit state }

              true ->
                { variable, state }
            end
        end

      # Module.function(args)
      dot_call(module, function, args, dotmeta, meta) when is_atom(module) and is_atom(function) and is_list(args) ->
        {args, state} = run(args, state)
        call = dot_call(module, function, args, dotmeta, meta)
        if precomputable?(call, state) do
          maybe_eval(call, state)
        else
          {call, state}
        end

      # Other calls
      {call, meta, args} ->
        {[call, args], state} = run([call, args], state)
        { {call, meta, args}, state }

      # Sanity checks
      variable when is_variable(variable, :_) ->
        raise "Came across underscore in normal context"

      tri ^_ ->
        raise "Came across pin in normal context"

      # Literals
      other ->
        if Macro.quoted_literal?(other) do
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

    clauses
    |> Enum.map(fn {:"->", meta, [left, right]} ->
      {left, left_state} = run_match(left, state)
      {Interpreter.multimatch(left, args), {left, left_state, meta, right}}
    end)
    |> filter_clauses()
    |> case do
      [{{:yes, bindings}, {_pattern, left_state, _meta, body}} | _] ->
        state = put_binds(state <~ left_state, bindings)
        body =
          bindings
          |> Enum.flat_map(fn {key, value} ->
            if Macro.quoted_literal?(value) or is_variable(value), do: [], else: [tri(key = value)]
          end)
          |> case do
            [] -> body
            lines -> {:__block__, [], lines ++ [body]}
          end

        {body, state} = run(body, state)
        {:block, body, hit(state)}

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
  end

  def run_guard(ast, state) do
    new_ast =
      Bindmap.unfold_while(ast, state.bindings, fn ast ->
        if is_guard(ast), do: :cont, else: :skip
      end)

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
            { pinned, new_state }

          {:ok, val} when is_variable(val) ->
            { pin(val, meta), new_state }

          {:ok, val} ->
            cond do
              Macro.quoted_literal?(val) ->
                { val, new_state }

              true ->
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

        {:rescue, clauses}, acc_state ->
          {clauses, acc_state} =
            Enum.map_reduce(clauses, acc_state, fn {:"->", arrowmeta, [[pattern], body]}, acc_state ->
              case pattern do
                {:in, inmeta, [variable, exception]} ->
                  {variable, pattern_state} = run_match(variable, state)
                  {body, body_state} = run(body, state <~ pattern_state)
                  {{:"->", arrowmeta, [[{:in, inmeta, [variable, exception]}], body]}, acc_state <~> body_state}

                tri %_{} ->
                  {body, body_state} = run(body, state)
                  {{:"->", arrowmeta, [[pattern], body]}, acc_state <~> body_state}

                variable when is_variable(variable) ->
                  {variable, pattern_state} = run_match(variable, state)
                  {body, body_state} = run(body, state <~ pattern_state)
                  {{:"->", arrowmeta, [[variable], body]}, acc_state <~> body_state}

                exception when is_atom(exception) ->
                  {body, body_state} = run(body, state)
                  {{:"->", arrowmeta, [[exception], body]}, acc_state <~> body_state}
              end
            end)

          {{:rescue, clauses}, acc_state}

        {catch_or_else, clauses}, acc_state when catch_or_else in ~w[catch else]a ->
          {clauses, acc_state} =
            Enum.map_reduce(clauses, acc_state, fn {:"->", meta, [pattern, body]}, acc_state ->
              {pattern, pattern_state} = run_match(pattern, state)
              {body, body_state} = run(body, state <~ pattern_state)
              {{:"->", meta, [pattern, body]}, acc_state <~> body_state}
            end)

          {{catch_or_else, clauses}, acc_state}
      end)

    {{:try, meta, [clauses]}, state}
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

  defp precomputable?(dot_call(module, function, args) = dc, _state) when is_atom(module) and is_atom(function) do
    with(
      true <- Enum.all?(args, &Macro.quoted_literal?/1),
      false <- Purity.check_analyze(dc)
    ) do
      match? {:pure, _, _}, Purity.run_analyze(dc)
    end
  end
  defp precomputable?(_ast, _state), do: false

  defp maybe_eval(call, state) do
    case Interpreter.eval(call) do
      {:ok, {result, []}} ->
        {Macro.escape(result), hit(state)}

      {:ok, {result, bindings}} ->
        block = {:__block__, [], Enum.map(bindings, fn {key, value} -> tri(key = value) end) ++ [result]}
        {block, hit(state)}

      _ ->
        {call, state}
    end
  end

  #TODO rename to something more meaningful
  defp tuplify([arg]), do: arg
  defp tuplify([left, right]), do: {left, right}
  defp tuplify(args) when is_list(args), do: {:{}, [], args}

  ## State helpers

  # defp get_bind(%{bindings: bindings}, key, default \\ nil) do
  #   Map.get(bindings, unmeta(key), default)
  # end

  defp fetch_bind(%{bindings: bindings}, key) do
    Bindmap.fetch_unfolded(bindings, key)
  end

  defp put_binds(%{bindings: bindings} = state, pairs) do
    %{state | bindings: Bindmap.put(bindings, pairs)}
  end

  defp put_bind(%{bindings: bindings} = state, key, value) do
    %{state | bindings: Bindmap.put(bindings, key, value)}
  end

  defp put_used(%{used: used} = state, key) do
    uid = :erlang.unique_integer([:positive])
    used =
      case used do
        %{^key => mapset} -> %{used | key => MapSet.put(mapset, uid)}
        _ -> Map.put(used, key, MapSet.new([uid]))
      end

    %{state | used: used}
  end

  defp unfold(value, %{bindings: bindings}) do
    Bindmap.unfold(value, bindings)
  end

  defp left <~ right do
    %{hit: left_hit, bindings: left_bindings, used: left_used}  = left
    %{hit: right_hit, bindings: right_bindings, used: right_used} = right

    bindings = Bindmap.merge!(left_bindings, right_bindings)
    used = Map.merge(left_used, right_used, fn _, l, r -> MapSet.union(l, r) end)

    %__MODULE__{used: used, bindings: bindings, hit: left_hit or right_hit}
  end

  defp left <~> right do
    %{hit: left_hit, used: left_used} = left
    %{hit: right_hit, used: right_used} = right

    used = Map.merge(left_used, right_used, fn _, l, r -> MapSet.union(l, r) end)

    %{left | used: used, hit: left_hit or right_hit}
  end

end
