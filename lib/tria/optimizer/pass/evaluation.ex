defmodule Tria.Optimizer.Pass.Evaluation do

  @moduledoc """
  !Runs on SSATranslator form only

  Pass which evaluates, propagates and drops local dead code in one run
  This pass performs almost no analysis and just tries to preevaluate any
  forms it comes accross.

  This pass stores the meta, but it still relies that asts with same meta are equal

  #TODO optimize try/rescue
  #TODO needs proper checking for possile literal

  For example
  ```elixir
  %{x: 1, y: y, z: fn -> send(pid, :x) end}
  last()
  ```

  Can be optimized to
  ```elixir
  last()
  ```
  """

  import Tria.Debug.Breakpoint, warn: false
  import Tria.Language
  import Tria.Language.Meta
  import Tria.Language.Tri
  alias Tria.Compiler.SSATranslator
  alias Tria.Debug.Tracer
  alias Tria.Language.Analyzer.Purity
  alias Tria.Language.Interpreter
  alias Tria.Language.Matchlist

  defstruct [
    bindings: %{}, # variable => value_ast in current context
    context: nil,
    hooks: %{}
  ]

  # Public

  def run_once!(ast, evaluation_context \\ []) do
    case run_once(ast, evaluation_context) do
      {:ok, new_ast} ->
        new_ast

      _ ->
        ast
    end
  end

  def run_once(ast, evaluation_context \\ [])
  def run_once(ast, evaluation_context) when is_list(evaluation_context) do
    run_once(ast, struct!(__MODULE__, evaluation_context))
  end
  def run_once(ast, evaluation_context) do
    # inspect_ast(ast, label: :running_over, with_contexts: true)
    {code, _evaluation_context} = run(ast, evaluation_context)
    case Process.delete(:hit) do
      true ->
        {:ok, code}

      _ ->
        {:error, :nothing_to_evaluate}
    end
  rescue
    e ->
      inspect_ast(ast, label: :run_once_fail)
      reraise e, __STACKTRACE__
  end

  # Bindings helpers

  # We do not return any binds in guard
  def fetch_bind(%{context: :guard}, _key), do: :error
  def fetch_bind(%{bindings: bindings}, key) do
    Map.fetch(bindings, unmeta key)
  end

  def put_bind(%{bindings: bindings} = evaluation_context, key, value) do
    %{evaluation_context | bindings: do_put_bind(bindings, key, value)}
  end

  def put_binds(%{bindings: bindings} = evaluation_context, binds) do
    bindings =
      Enum.reduce(binds, bindings, fn {key, value}, bindings ->
        do_put_bind(bindings, key, value)
      end)

    %{evaluation_context | bindings: bindings}
  end

  defp do_put_bind(bindings, {name, _, _}, {bind_to_name, _, _}) when :_ in [name, bind_to_name], do: bindings
  defp do_put_bind(bindings, key, value) do
    key = fold(key, bindings)
    value = fold(value, bindings)
    if key != value do
      Map.put(bindings, key, value)
    else
      bindings
    end
  end

  def fold(value, bindings) do
    value
    |> unmeta()
    |> postwalk(&Map.get(bindings, &1, &1))
  end

  # Implementation

  # This could've been a function, but it is a macro for debugging
  # defmacrop hit do
  #   quote do
  #     Process.put(:hit, true)
  #     IO.puts "Hit on #{unquote __CALLER__.line}"
  #     Process.sleep(10)
  #   end
  # end

  # I use process dictionary just because optimization we need to know if anything
  # was optimized
  defp hit, do: Process.put(:hit, true)

  # Main recursive function
  defp run(code, evaluation_context) do
    {code, evaluation_context} = run_hook(code, evaluation_context, :before)
    case code do
      # Breakpoint
      breakpoint(point) ->
        handle_breakpoint(point)

      # Block
      {:__block__, meta, block} ->
        {lines, new_evaluation_context} =
          Enum.flat_map_reduce(block, evaluation_context, fn line, evaluation_context ->
            {line, evaluation_context} = run(line, evaluation_context)
            case line do
              {:__block__, _, lines} ->
                {lines, evaluation_context}

              line ->
                {[line], evaluation_context}
            end
          end)

        block =
          evaluation_context
          |> drop_literals_in_block(lines)
          |> case do
            [] -> raise "This should never happen"
            [line] -> line
            lines -> {:__block__, meta, lines}
          end

        {block, new_evaluation_context}


      # Equals
      tri(left = right) = equation ->
        {left, left_evaluation_context} = run_pattern(left, evaluation_context)
        {right, right_evaluation_context} = run(right, evaluation_context)

        case Interpreter.match(left, right) do
          {:yes, binds} ->
            {evaluation_context, binds} =
              Enum.reduce(binds, {evaluation_context, []}, fn
                {variable, value} = bind, {evaluation_context, binds} ->
                  cond do
                    Macro.quoted_literal?(value) ->
                      {put_bind(evaluation_context, variable, value), binds}

                    is_variable(value) ->
                      {put_bind(evaluation_context, variable, value), [bind_to_equal(bind) | binds]}

                    is_fn(value) ->
                      {put_bind(evaluation_context, variable, value), [bind_to_equal(bind) | binds]}

                    true ->
                      {put_bind(evaluation_context, variable, value), [bind_to_equal(bind) | binds]}
                  end
              end)

            Tracer.tag(evaluation_context, label: :after_binds)

            body =
              case binds do
                # No binds, hmmm.
                # In case this is something like `1 = 1`
                [] ->
                  hit()
                  right

                [bind] ->
                  # There was one bind
                  # There is one bind
                  if unmeta(bind) != unmeta(equation) do
                    hit()
                  end
                  bind

                # In case there are multiple binds
                binds ->
                  hit()
                  {:__block__, [], binds}
              end

            evaluation_context = evaluation_context <~ left_evaluation_context
            {body, evaluation_context}

          :no ->
            hit()
            IO.warn "This bind will never match"
            body = quote do: raise MatchError, term: unquote right
            {body, evaluation_context <~ right_evaluation_context <~ left_evaluation_context}

          {:maybe, _} ->
            #TODO relax matching
            {
              quote(do: unquote(left) = unquote(right)),
              evaluation_context <~ right_evaluation_context <~ left_evaluation_context
            }
        end

      # Case
      tri case(arg, do: clauses) ->
        {arg, evaluation_context} = run(arg, evaluation_context)
        code = optimize_clauses([arg], clauses, evaluation_context, fn [arg] -> quote do: raise CaseClauseError, term: unquote arg end)
        {code, evaluation_context}

      # Fn Inlining
      {{:".", _dotmeta, [{:fn, _, clauses}]}, _meta, args} ->
        hit() # This one is always optimized into case
        #TODO this doesn't have to be always optimized into case. Sometimes inlined fn is faster

        {args, evaluation_context} = run(args, evaluation_context)
        code =
          optimize_clauses(args, clauses, evaluation_context, fn args ->
            quote do: raise FunctionClauseError, args: unquote(args)
          end)
        {code, evaluation_context}

      # Fn
      {:fn, meta, clauses} ->
        clauses =
          clauses
          |> Enum.map(fn {:"->", meta, [left, right]} ->
            {left, left_evaluation_context} = run_pattern(left, evaluation_context)
            {right, _right_evaluation_context} = run(right, evaluation_context <~ left_evaluation_context)

            {:"->", meta, [left, right]}
          end)

        {{:fn, meta, clauses}, evaluation_context}

      # For
      {:for, meta, iters_opts_body} ->
        {opts_body, iters} = List.pop_at(iters_opts_body, -1)
        {body, opts} = Keyword.pop!(opts_body, :do)

        {iters, inner_evaluation_context} = run_left_arrow_clauses(iters, evaluation_context)
        {opts, inner_evaluation_context} = run(opts, inner_evaluation_context)

        body =
          case body do
            {:"->", meta, [left, right]} ->
              {left, left_evaluation_context} = run_pattern(left, inner_evaluation_context)
              {right, _right_evaluation_context} = run(right, inner_evaluation_context <~ left_evaluation_context)
              {:"->", meta, [left, right]}

            body ->
              {body, _evaluation_context} = run(body, inner_evaluation_context)
              body
          end

        {{:for, meta, iters ++ [opts ++ [do: body]]}, evaluation_context}

      # With
      #FIXME add ability to parse arbitary statements in with
      {:with, meta, clauses} ->
        {[{:do, do_clause} | else_clause?], clauses} = List.pop_at(clauses, -1)
        else_clauses = Keyword.get(else_clause?, :else, [])

        {clauses, do_evaluation_context} = run_left_arrow_clauses(clauses, evaluation_context)
        {do_clause, _} = run(do_clause, do_evaluation_context)

        # #TODO optimize else_clauses
        # {tri(case(_, do: else_clauses)), _} =
        #   run(quote(do: case(x, do: unquote(else_clauses))), evaluation_context)
        case else_clauses do
          e when e in [[], nil] ->
            {{:with, meta, clauses ++ [[do: do_clause]]}, evaluation_context}

          else_clauses ->
            {{:with, meta, clauses ++ [[do: do_clause, else: else_clauses]]}, evaluation_context}
        end

      # Map cons
      # TODO think about joining in map cons
      {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs1]} | pairs2]} ->
        pairs = pairs1 ++ pairs2
        {map, map_evaluation_context} = run(map, evaluation_context)
        {pairs, evaluation_context} =
          Enum.map_reduce(pairs, map_evaluation_context, fn {key, value}, new_evaluation_context ->
            {key, key_evaluation_context} = run(key, evaluation_context)
            {value, value_evaluation_context} = run(value, evaluation_context)
            {{key, value}, new_evaluation_context <~ key_evaluation_context <~ value_evaluation_context}
          end)

        code =
          case map do
            {:"%{}", meta, items} ->
              #TODO implement uniqualization of the items
              {:"%{}", meta ++ map_meta ++ cons_meta, items ++ pairs}

            _ ->
              {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]}
          end

        {code, evaluation_context}

      # Map
      tri %{tri_splicing pairs} ->
        {pairs, evaluation_context} =
          Enum.map_reduce(pairs, evaluation_context, fn {key, value}, new_evaluation_context ->
            {key, key_evaluation_context} = run(key, evaluation_context)
            {value, value_evaluation_context} = run(value, evaluation_context)
            {{key, value}, new_evaluation_context <~ key_evaluation_context <~ value_evaluation_context}
          end)

        {
          quote(do: %{unquote_splicing(pairs)}),
          evaluation_context
        }

      # Binary
      {:"<<>>", meta, items} ->
        # Matchlist.inspect(evaluation_context.bindings, label: :bindings_in_binary, with_contexts: true)
        {items, evaluation_contexts} =
          Enum.map(items, fn
            # TODO find out if the propagation is required for the right side
            # UPD: It is required, but I am not sure if it is safe just to propagate to the right side
            {:"::", meta, [item, type]} ->
              {item, new_evaluation_context} = run(item, evaluation_context)
              {type, _type_evaluation_context} = run(type, evaluation_context)
              {{:"::", meta, [item, type]}, new_evaluation_context}

            item ->
              run(item, evaluation_context)
          end)
          |> Enum.unzip()

        {{:"<<>>", meta, items}, merge_evaluation_contexts(evaluation_contexts)}

      # Empty list
      [] ->
        {[], evaluation_context}

      # List
      items when is_list(items) ->
        {items, evaluation_contexts} =
          items
          |> Enum.map(fn item -> run(item, evaluation_context) end)
          |> Enum.unzip()

        # Handle cons
        items =
          case List.pop_at(items, -1) do
            {{:"|", _, [left, right]}, head} when is_list(right) ->
              head ++ [left] ++ right

            _ ->
              items
          end

        {items, merge_evaluation_contexts(evaluation_contexts)}

      # Twople
      {left, right} ->
        {left, left_evaluation_context} = run(left, evaluation_context)
        {right, right_evaluation_context} = run(right, evaluation_context)
        {{left, right}, left_evaluation_context <~ right_evaluation_context}

      # Tuple
      tri {tri_splicing items} ->
        {items, evaluation_context} = run(items, evaluation_context)
        {quote(do: {unquote_splicing(items)}), evaluation_context}

      # MFA dot_call Dotcall
      dot_call(module, func, args) when is_atom(module) and is_atom(func) ->
        {args, args_evaluation_context} = run(args, evaluation_context)
        call = dot_call(module, func, args)
        if Enum.all?(args, &Macro.quoted_literal?/1) and precomputable?(evaluation_context, call) do
          {maybe_eval(call), args_evaluation_context}
        else
          {call, args_evaluation_context}
        end

      # Dot
      # TODO evaluate something like `%{x: 1}.x`
      {{:., dotmeta, dot}, meta, args} ->
        {dot, dot_evaluation_context} = run(dot, evaluation_context)
        {args, args_evaluation_context} = run(args, evaluation_context)

        {
          {{:., dotmeta, dot}, meta, args},
          dot_evaluation_context <~ args_evaluation_context
        }

      # Function call
      {func, meta, args} when is_list(args) ->
        {args, args_evaluation_context} = run(args, evaluation_context)
        call = {func, meta, args}
        if Enum.all?(args, &Macro.quoted_literal?/1) and precomputable?(evaluation_context, call) do
          {maybe_eval(call), args_evaluation_context}
        else
          {call, args_evaluation_context}
        end

      # Variable
      variable when is_variable(variable) ->
        Tracer.tag(variable, label: :variable)
        Tracer.tag(evaluation_context, label: :context)

        evaluation_context
        |> fetch_bind(variable)
        |> Tracer.tag(label: :fetch_bind_result)
        |> case do
          :error ->
            {variable, evaluation_context}

          {:ok, value} ->
            cond do
              is_variable(value) ->
                hit()
                {value, evaluation_context}

              Macro.quoted_literal?(value) ->
                hit()
                {value, evaluation_context}

              is_fn(value) ->
                hit()
                # When inlining `fn`-s we want to rearrange static assignments
                value = SSATranslator.from_tria(value)
                {value, evaluation_context}

              true ->
                {variable, evaluation_context}
            end
        end

      # Literal
      other ->
        {other, evaluation_context}
    end
    |> run_hook(:after)
  end

  ## Clauses helpers

  defp run_left_arrow_clauses(clauses, evaluation_context) do
    Enum.map_reduce(clauses, evaluation_context, fn
      {:"<-", meta, [left, right]}, evaluation_context ->
        {left, left_evaluation_context} = run_pattern(left, evaluation_context)
        {right, _right_evaluation_context} = run(right, evaluation_context)
        { {:"<-", meta, [left, right]}, evaluation_context <~ left_evaluation_context }

      other, evaluation_context ->
        {other, _evaluation_context} = run(other, evaluation_context)
        {other, evaluation_context}
    end)
  end

  ## Propagating to pattern

  # This function just puts values into the pattern and translates the contexts in the pattern
  # to make the SSA form of the code
  def run_pattern(code, evaluation_context, new_evaluation_context \\ %__MODULE__{}) do
    case code do
      # When
      {:when, meta, pattern_and_guard} ->
        {guard, patterns} = List.pop_at(pattern_and_guard, -1)
        {patterns, pattern_evaluation_context} = run_pattern(patterns, evaluation_context, new_evaluation_context)

        # Since guard is not creating new variables, we can just run evaluation against it
        {guard, _guard_evaluation_context} = run(guard, %{evaluation_context <~ pattern_evaluation_context | context: :guard})
        {{:when, meta, patterns ++ [guard]}, pattern_evaluation_context}

      # Pin . We handle pin separately before variables
      {:^, _, [variable]} = pinned when is_variable(variable) ->
        case fetch_bind(evaluation_context, variable) do
          :error ->
            {pinned, new_evaluation_context}

          {:ok, val} when is_variable(val) ->
            hit()
            {{:^, [], [val]}, new_evaluation_context}

          {:ok, val} ->
            if Macro.quoted_literal?(val) do
              hit()
              {val, new_evaluation_context}
            else
              {pinned, new_evaluation_context}
            end
        end

      # Map Tuple Binary or matchable operator <>
      {n, m, items} when is_list(items) ->
        {items, new_evaluation_context} = run_pattern(items, evaluation_context, new_evaluation_context)
        {{n, m, items}, new_evaluation_context}

      # List
      items when is_list(items) ->
        Enum.map_reduce(items, new_evaluation_context, fn item, acc_evaluation_context ->
          run_pattern(item, evaluation_context, acc_evaluation_context)
        end)

      # Twople
      {left, right} ->
        {left, new_evaluation_context} = run_pattern(left, evaluation_context, new_evaluation_context)
        {right, new_evaluation_context} = run_pattern(right, evaluation_context, new_evaluation_context)

        {{left, right}, new_evaluation_context}

      # Literal Variable
      literal_or_variable when is_literal(literal_or_variable) or is_variable(literal_or_variable) ->
        {literal_or_variable, new_evaluation_context}
    end
  end

  ## Helpers for working with evaluation_context

  defp merge_evaluation_contexts([]), do: %__MODULE__{}
  defp merge_evaluation_contexts(evaluation_contexts) when is_list(evaluation_contexts) do
    Enum.reduce(evaluation_contexts, fn right, left -> left <~ right end)
  end

  defp left <~ right do
    %{bindings: left_bindings,  context: left_context}  = left
    %{bindings: right_bindings, context: right_context} = right

    bindings =
      Map.merge(left_bindings, right_bindings, fn
        # This fn is just a check to assure that contexts do not have conflicts
        _, v, v -> v
        _, _, _ ->
          IO.inspect(left_bindings, label: :left_bindings)
          IO.inspect(right_bindings, label: :right_bindings)
          raise "What the fuck"
      end)

    %{left | context: left_context || right_context, bindings: bindings}
  end

  defp precomputable?(_evaluation_context, dot_call(module, _, args) = dc) when is_atom(module) do
    with(
      true <- Enum.all?(args, &Macro.quoted_literal?/1),
      false <- Purity.check_analyze(dc)
    ) do
      match? {:pure, _, _}, Purity.run_analyze(dc)
    end
  end
  defp precomputable?(_evaluation_context, _ast), do: false

  ## Hooks

  defp run_hook({code, evaluation_context}, hookname), do: run_hook(code, evaluation_context, hookname)
  defp run_hook(code, %__MODULE__{bindings: bindings, hooks: hooks} = evaluation_context, hookname) do
    case hooks do
      %{^hookname => {hook, acc}} ->
        {code, acc} = hook.(code, acc, bindings)
        {code, %{evaluation_context | hooks: %{hooks | hookname => {hook, acc}}}}

      %{^hookname => hook} ->
        {hook.(code, bindings), evaluation_context}

      _ ->
        {code, evaluation_context}
    end
  end

  ## Helpers optimizations

  defp optimize_clauses(args, clauses, evaluation_context, error) do
    clauses
    |> Enum.map(fn {:"->", meta, [left, right]} ->
      {left, left_evaluation_context} = run_pattern(left, evaluation_context)
      {Interpreter.multimatch(left, args), {left, left_evaluation_context, meta, right}}
    end)
    |> filter_clauses()
    |> case do
      [{{:yes, bindings}, {_pattern, left_evaluation_context, _meta, body}}] ->
        hit()
        evaluation_context = put_binds(evaluation_context, bindings)

        {body, inner_evaluation_context} = run(body, evaluation_context <~ left_evaluation_context)
        fold(body, inner_evaluation_context.bindings)

      [] ->
        # Nothing will ever match in this case
        # TODO proper raise

        hit()
        IO.warn "This fn will never success"
        error.(args)

      confidence_and_clauses ->
        clauses =
          Enum.map(confidence_and_clauses, fn {{_confidence, bindings}, {left, left_evaluation_context, clause_meta, body}} ->
            evaluation_context = put_binds(evaluation_context, bindings)
            {right, _} = run(body, evaluation_context <~ left_evaluation_context)

            case left do
              [{:when, meta, pattern_and_guard}] ->
                {guard, pattern} = List.pop_at(pattern_and_guard, -1)
                {:->, clause_meta, [[{:when, meta, [maybe_tuplify(pattern), guard]}], right]}

              pattern ->
                {:->, clause_meta, [[maybe_tuplify(pattern)], right]}
            end
          end)

        quote do: case(unquote(maybe_tuplify args), do: unquote clauses)
    end
  end

  defp drop_literals_in_block(_evaluation_context, []), do: []
  defp drop_literals_in_block(_evaluation_context, [last]), do: [last]
  defp drop_literals_in_block(evaluation_context, [first, second | tail]) do
    if Macro.quoted_literal?(first) or is_fn(first) or is_variable(first) do
      hit()
      drop_literals_in_block(evaluation_context, [second | tail])
    else
      [first | drop_literals_in_block(evaluation_context, [second | tail])]
    end
  end

  defp filter_clauses([{{:yes, _}, _} = last | tail]) do
    if tail != [], do: hit()
    [last]
  end
  defp filter_clauses([{:no, _} | tail]) do
    hit()
    filter_clauses(tail)
  end
  defp filter_clauses([other | tail]), do: [other | filter_clauses(tail)]
  defp filter_clauses([]), do: []

  ## General helpers

  defp maybe_eval(call) do
    import Matchlist, only: [is_empty: 1]
    case Interpreter.eval(call) do
      {:ok, {result, matchlist}} when is_empty(matchlist) ->
        hit()
        Macro.escape(result)

      _ ->
        call
    end
  end

  defp maybe_tuplify([one]), do: one
  defp maybe_tuplify(many), do: {:"{}", [], many}

  defp bind_to_equal({left, right}) do
    quote do: unquote(left) = unquote(right)
  end

end
