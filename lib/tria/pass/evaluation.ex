defmodule Tria.Pass.Evaluation do

  @moduledoc """
  Pass which evaluates, propagates and drops dead code in one run
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

  import Tria.Common
  import Tria.Tri
  alias Tria.Interpreter
  alias Tria.Analyzer.Purity

  defstruct [
    bindings: %{}, # Maps current bindings is context
    hit?: false,   # Checks if the evaluation hit any optimization
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
    {code, _bindings} = do_run(ast, evaluation_context)
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

  def fetch_bind(%{bindings: bindings} = cfg, {name, _, context}) do
    # Trick to fetch nested binds
    with(
      {:ok, var} when is_variable(var) <- Map.fetch(bindings, {name, context}),
      :error <- fetch_bind(cfg, var)
    ) do
      {:ok, var}
    end
  end

  def put_bind(cfg, {name, _, _}, {bind_to_name, _, _}) when :_ in [name, bind_to_name], do: cfg
  def put_bind(%{bindings: bindings} = cfg, {name, _, context}, value) do
    %{cfg | bindings: Map.put(bindings, {name, context}, value)}
  end

  # Implementation

  # # This could've been a function, but it is a macro for debugging
  # defmacrop hit do
  #   quote do
  #     Process.put(:hit, true)
  #     # IO.puts "Hit on #{unquote __CALLER__.line}"
  #     # Process.sleep(10)
  #   end
  # end

  # I use process dictionary just because optimization can hit the code at least once
  defp hit, do: Process.put(:hit, true)

  # Main recursive function
  defp do_run(code, evaluation_context) do
    {code, evaluation_context} = run_hook(code, evaluation_context, :before)
    case code do
      # Block
      {:__block__, meta, block} ->
        {lines, new_evaluation_context} =
          Enum.flat_map_reduce(block, evaluation_context, fn line, evaluation_context ->
            {line, evaluation_context} = do_run(line, evaluation_context)
            lines = List.wrap with {:__block__, _, block} <- line, do: block
            {lines, evaluation_context}
          end)

        block =
          evaluation_context
          |> drop_literals_in_block(lines)
          |> case do
            [] -> raise "Somebody fucked up"
            [line] -> line
            lines -> {:__block__, meta, lines}
          end

        {block, new_evaluation_context}


      # Equals
      tri(left = right) ->
        {hit?, body, env} = run_equals(left, right, evaluation_context)
        if hit? do
          hit()
        end
        {body, env}

      # Case
      tri(case(arg, do: clauses)) ->
        {arg, evaluation_context} = do_run(arg, evaluation_context)
        code = optimize_clauses([arg], clauses, evaluation_context)
        {code, evaluation_context}

      # Fn inlining
      # I can't remember why I left dotted call here
      {{:".", _, [{:fn, _, clauses}]}, _, args} ->
        hit() # This one is always optimized into case
        {args, evaluation_context} = do_run(args, evaluation_context)
        code = optimize_clauses(args, clauses, evaluation_context)
        {code, evaluation_context}

      # Fn
      {:fn, _, clauses} ->
        clauses =
          clauses
          |> Enum.map(fn {:"->", _, [left, right]} ->
            {left, new_evaluation_context} = propagate_to_pattern(left, evaluation_context)
            {right, _right_evaluation_context} = do_run(right, evaluation_context <~ new_evaluation_context)

            {:"->", [], [left, right]}
          end)

        {{:fn, [], clauses}, evaluation_context}

      # For
      {:for, _meta, _iters_and_stuff} ->
        raise "Not implemented yet"

      # With
      #FIXME add ability to parse arbitary statements in with
      {:with, meta, clauses} ->
        {[{:do, do_clause} | else_clause?], clauses} = List.pop_at(clauses, -1)
        else_clauses = Keyword.get(else_clause?, :else, [])

        {clauses, do_evaluation_context} =
          Enum.map_reduce(clauses, evaluation_context, fn {op, meta, [left, right]}, evaluation_context ->
            {right, _right_conf} = do_run(right, evaluation_context)
            {left, left_evaluation_context} = propagate_to_pattern(left, evaluation_context)

            {
              {op, meta, [left, right]},
              evaluation_context <~ left_evaluation_context
            }
          end)

        {do_clause, _} = do_run(do_clause, do_evaluation_context)

        # #TODO optimize else_clauses
        # {tri(case(_, do: else_clauses)), _} =
        #   do_run(quote(do: case(x, do: unquote(else_clauses))), evaluation_context)
        case else_clauses do
          e when e in [[], nil] ->
            {{:with, meta, clauses ++ [[do: do_clause]]}, evaluation_context}

          else_clauses ->
            {{:with, meta, clauses ++ [[do: do_clause, else: else_clauses]]}, evaluation_context}
        end

      # Map cons
      # TODO think about joining in map cons
      {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]} ->
        {map, map_evaluation_context} = do_run(map, evaluation_context)
        {pairs, evaluation_context} =
          Enum.map_reduce(pairs, map_evaluation_context, fn {key, value}, new_evaluation_context ->
            {key, key_evaluation_context} = do_run(key, evaluation_context)
            {value, value_evaluation_context} = do_run(value, evaluation_context)
            {{key, value}, new_evaluation_context <~ key_evaluation_context <~ value_evaluation_context}
          end)
        {
          {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]},
          evaluation_context
        }

      # Map
      tri %{tri_splicing pairs} ->
        {pairs, evaluation_context} =
          Enum.map_reduce(pairs, evaluation_context, fn {key, value}, new_evaluation_context ->
            {key, key_evaluation_context} = do_run(key, evaluation_context)
            {value, value_evaluation_context} = do_run(value, evaluation_context)
            {{key, value}, new_evaluation_context <~ key_evaluation_context <~ value_evaluation_context}
          end)

        {
          quote(do: %{unquote_splicing(pairs)}),
          evaluation_context
        }

      # Binary
      tri <<tri_splicing items>> ->
        {items, evaluation_contexts} =
          Enum.map(items, fn
            # TODO find out if the propagation is required for the right side
            tri(item :: type) ->
              {item, new_evaluation_context} = do_run(item, evaluation_context)
              {quote(do: unquote(item) :: unquote(type)), new_evaluation_context}

            item ->
              do_run(item, evaluation_context)
          end)
          |> Enum.unzip()

        {quote(do: <<unquote_splicing(items)>>), merge_evaluation_contexts(evaluation_contexts)}

      # List
      items when is_list(items) ->
        {items, evaluation_contexts} =
          items
          |> Enum.map(fn item -> do_run(item, evaluation_context) end)
          |> Enum.unzip()

        {items, merge_evaluation_contexts(evaluation_contexts)}

      # Twople
      {left, right} ->
        {left, left_evaluation_context} = do_run(left, evaluation_context)
        {right, right_evaluation_context} = do_run(right, evaluation_context)
        {{left, right}, left_evaluation_context <~ right_evaluation_context}

      # Tuple
      tri {tri_splicing(items)} ->
        {items, evaluation_context} = do_run(items, evaluation_context)
        {quote(do: {unquote_splicing(items)}), evaluation_context}

      # MFA dot_call Dotcall
      dot_call(module, func, args) ->
        {args, args_evaluation_context} = do_run(args, evaluation_context)
        call = dot_call(module, func, args)
        if Enum.all?(args, &Macro.quoted_literal?/1) and precomputable?(evaluation_context, call) do
          {maybe_eval(call), args_evaluation_context}
        else
          {call, args_evaluation_context}
        end

      # Dot
      {{:., dotmeta, dot}, meta, args} ->
        {dot, dot_evaluation_context} = do_run(dot, evaluation_context)
        {args, args_evaluation_context} = do_run(args, evaluation_context)

        {
          {{:., dotmeta, dot}, meta, args},
          dot_evaluation_context <~ args_evaluation_context
        }

      # Function call
      {func, meta, args} when is_list(args) ->
        {args, args_evaluation_context} = do_run(args, evaluation_context)
        call = {func, meta, args}
        if Enum.all?(args, &Macro.quoted_literal?/1) and precomputable?(evaluation_context, call) do
          {maybe_eval(call), args_evaluation_context}
        else
          {call, args_evaluation_context}
        end

      # Variable
      var when is_variable(var) ->
        case fetch_bind(evaluation_context, var) do
          :error ->
            {var, evaluation_context}

          {:ok, variable} when is_variable(variable) ->
            {variable, evaluation_context}

          {:ok, value} ->
            hit()
            {value, evaluation_context}
        end

      # Literal
      other ->
        {other, evaluation_context}
    end
    |> run_hook(:after)
  end

  @spec run_equals(term, term, term) :: {boolean, Macro.t(), term}
  def run_equals(left, right, env) do
    {left, lenv} = propagate_to_pattern(left, env)
    {right, renv} = do_run(right, env)

    case Interpreter.match(left, right) do
      :no ->
        IO.warn "This bind will never match"
        body = quote do: raise "Will never match"
        {
          true,
          body,
          env <~ renv <~ lenv
        }

      {:maybe, _} ->
        #TODO relax matching
        {
          false,
          quote(do: unquote(left) = unquote(right)),
          env <~ renv <~ lenv
        }

      {:yes, bindings} ->
        {hit?, new_env, binds} = from_interpreter_binds(env <~ lenv, bindings)

        body =
          case binds do
            # No binds, hmmm.
            [] ->
              right

            # This is just a `variable = something()` case
            [bind] ->
              bind

            binds ->
              quote do
                unquote_splicing(binds)
                unquote(right)
              end
          end

        {hit?, body, new_env}
    end
  end

  @spec from_interpreter_binds(term, [Interpreter.binding()]) :: {boolean, term, list}
  defp from_interpreter_binds(env, binds) do
    Enum.reduce(binds, {false, env, []}, &from_interpreter_bind/2)
  end

  @spec from_interpreter_bind(Interpreter.binding(), {boolean, term, list}) :: {boolean, term, list}
  defp from_interpreter_bind({var, val} = bind, {hit?, env, binds}) do
    cond do
      Macro.quoted_literal?(val) ->
        {hit?, put_bind(env, var, val), binds}

      is_variable(val) ->
        {true, put_bind(env, var, val), binds}

      is_fun(val) ->
        {hit?, put_bind(env, var, val), [bind_to_equal(bind) | binds]}

      true ->
        {true, env, [bind_to_equal(bind) | binds]}
    end
  end

  # Propagating to pattern

  defp propagate_to_pattern(code, evaluation_context, new_evaluation_context \\ %__MODULE__{}) do
    case code do
      # When
      {:when, meta, pattern_and_guard} ->
        {guard, patterns} = List.pop_at(pattern_and_guard, -1)
        {patterns, pattern_evaluation_context} = propagate_to_pattern(patterns, evaluation_context, new_evaluation_context)

        # Since guard is not creating new variables, we can just run evaluation against it
        {guard, _guard_evaluation_context} = do_run(guard, evaluation_context <~ pattern_evaluation_context)

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

      # Variable
      variable when is_variable(variable) ->
        case fetch_bind(new_evaluation_context, variable) do
          :error ->
            # It appears that it is the new variable
            new_variable = unify(variable)
            {new_variable, put_bind(new_evaluation_context, variable, new_variable)}

          {:ok, variable} ->
            # It appears that the variable is present multiple times in the pattern
            {variable, new_evaluation_context}
        end

      # Map Tuple Binary or matchable operator <>
      {n, m, items} when is_list(items) ->
        {items, new_evaluation_context} = propagate_to_pattern(items, evaluation_context, new_evaluation_context)
        {{n, m, items}, new_evaluation_context}

      # List
      items when is_list(items) ->
        Enum.map_reduce(items, new_evaluation_context, fn item, acc_evaluation_context ->
          propagate_to_pattern(item, evaluation_context, acc_evaluation_context)
        end)

      # Twople
      {left, right} ->
        {left, new_evaluation_context} = propagate_to_pattern(left, evaluation_context, new_evaluation_context)
        {right, new_evaluation_context} = propagate_to_pattern(right, evaluation_context, new_evaluation_context)

        {{left, right}, new_evaluation_context}

      # Literal
      literal when is_literal(literal) ->
        {literal, new_evaluation_context}
    end
  end

  ## Helpers for working with evaluation_context

  defp merge_evaluation_contexts([]), do: %__MODULE__{}
  defp merge_evaluation_contexts(evaluation_contexts) when is_list(evaluation_contexts) do
    Enum.reduce(evaluation_contexts, fn right, left -> left <~ right end)
  end

  defp (%{bindings: left_bindings} = left) <~ (%{bindings: right_bindings}) do
    %{left | bindings: Map.merge(left_bindings, right_bindings)}
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

  defp unify({varname, meta, _cotnext}) do
    {varname, meta, gen_uniq_context()}
  end

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

  defp optimize_clauses(args, clauses, evaluation_context) do
    clauses
    |> Enum.map(fn {:"->", meta, [left, right]} ->
      {left, left_evaluation_context} = propagate_to_pattern(left, evaluation_context)
      {Interpreter.multimatch(left, args), {left, left_evaluation_context, meta, right}}
    end)
    |> filter_clauses()
    |> case do
      [{{:yes, bindings}, {_pattern, left_evaluation_context, _meta, body}}] ->
        hit()
        evaluation_context =
          Enum.reduce(bindings, evaluation_context <~ left_evaluation_context, fn {key, value}, evaluation_context when is_variable(key) ->
            put_bind(evaluation_context, key, value)
          end)

        {body, _inner_evaluation_context} = do_run(body, evaluation_context)
        body

      [] ->
        hit()
        # Nothing will ever match in this case
        # FIXME proper raise
        IO.warn "This fn will never success"
        quote(do: raise "Will never match")

      confidence_and_clauses ->
        clauses =
          Enum.map(confidence_and_clauses, fn {{_confidence, _bindings}, {left, left_evaluation_context, clause_meta, body}} ->
            {right, _right_evaluation_context} = do_run(body, evaluation_context <~ left_evaluation_context)
            case left do
              [{:when, meta, pattern_and_guard}] ->
                {guard, pattern} = List.pop_at(pattern_and_guard, -1)
                {:->, clause_meta, [[{:when, meta, [maybe_tuplify(pattern), guard]}], right]}

              pattern ->
                {:->, clause_meta, [[maybe_tuplify(pattern)], right]}
            end
          end)

        quote do: case(unquote(maybe_tuplify args), do: unquote(clauses))
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
    case Interpreter.eval(call) do
      {:ok, {result, []}} ->
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
