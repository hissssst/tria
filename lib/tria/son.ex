defmodule Tria.Son do
  @moduledoc """
  Sea of nodes implementation

  ## Uncasing

  1. Transform pattern matching into a

  ```
  {x, {y, z}, _, 1} = {f(), g(), h(), :constant, function()}

  # is translated to

  fres = f()
  gres = g()
  hres = h()
  x = fres
  1 = function()
  ( {y, _} = gres ; y )
  ( {_, z} = hres ; z ) # and the usages of the variable will point here
  {fres, gres, hres, :constant}
  ```

  ## Top-level layer

  it compiles to Elixir and has these special nodes

  1. `literal`
  Just a literal expression

  2. `pattern`
  pattern-matching is just an equals expression with one variable `{:target, _, :tria}`
  inside like `{target, _}` which points to another expression as an argument.
  For example `{target, _} --arg1--> x` represents something like `{target, _} = x`

  3. `call`
  represents `Module.function/arity` call, pointing to it's arguments

  4. `structure_expression`
  represents basic structure expression, like `[{1, 2}, 1]` represents expression
  like `[{arg(1), arg(2)}, arg(1)]`

  5. `case`
  respresents control node, similar to case. Points to arg and clauses

  ## Vocabulary

  Relaxing — splitting one complex node into a set of smaller, but easier ones.
  This is usually done to extend available code motions

  Straining — process of combining several nodes into a one complex node.
  This is intended to be used during compilation to high level languages
  """

  alias Tria.Language.Analyzer
  alias Tria.Language.Interpreter
  alias Tria.Son.Graph
  import Tria.Language
  import Tria.Language.Meta, only: [unmeta: 1]
  import Tria.Language.Tri

  def test do
    ssa =
      tri to_ssa: true do
        {meta, path, mod, env} = MapSet.new()
        case Keyword.fetch(meta, :import) do
          {:ok, Pathex} ->
            {:ok, path, Pathex.maybemod(mod)}

          :error ->
            case Macro.Env.lookup_import(env, {:path, 2}) do
              [{:macro, Pathex} | _] ->
                {:ok, path, Pathex.maybemod(mod)}

              _ ->
                :error
            end

          _ ->
            :error
        end
      end

    ssa
    |> inspect_ast(label: :ssa)
    |> translate()
    |> Graph.visualize()
  end

  defp to_matches(tria) do
    tria
    |> filterwalk(&is_variable/1)
    |> MapSet.new(&unmeta/1)
    |> MapSet.reject(&match?({:_, _, _}, &1))
    |> Enum.map(fn variable ->
      target = {:target, [], gen_uniq_context()}
      value =
        prewalk(tria, fn
          v when is_variable(v) ->
            case unmeta(v) do
              ^variable -> target
              _ -> {:_, [], Elixir}
            end

          other ->
            other
        end)

      {variable, {:pattern, value}}
    end)
  end

  def translate(ssa_ast, context \\ []) do
    state = %{vi: %{}, graph: %Graph{}}
    state =
      Enum.reduce(context, state, fn variable, %{vi: vi, graph: graph} ->
        {id, graph} = Graph.add_vertex(graph, :input)
        vi = Map.put(vi, variable, id)
        %{vi: vi, graph: graph}
      end)

    {id, state} =
      ssa_ast
      |> unmeta()
      |> translate(:start, state)

    %{graph: graph} = deduplicate cleanup state

    Graph.add_edge(graph, :end, id, {:arg, 0})
  end

  def translate(ssa_ast, start, %{vi: vi, graph: graph} = state) do
    case ssa_ast do
      # Variable
      variable when is_variable(variable) ->
        id = Map.fetch!(vi, unmeta(variable))
        {id, state}

      # Equals
      # It is tricky because, pattern matching is expressed via `:pattern` kind of vertex
      # And whole equals is an expression itself, which returns the right side
      {:=, _meta, [left, right]} ->
        # Here we convert match into a list of separate matches and link those who have effects
        # This operation relaxes the matching to provide more code motion
        {_level, matches} = Interpreter.match(left, right)
        {matches, {_prefinal_id, %{graph: graph} = state}} =
          Enum.map_reduce(matches, {start, state}, fn {pattern, value}, {start, %{} = state} ->
            pure? = Analyzer.is_pure(value)
            {value_id, state} = translate(value, start, state)
            state =
              if pure? do
                state
              else
                %{state | graph: Graph.add_edge(state.graph, start, value_id, :before)}
              end

            {{pattern, value, value_id}, {value_id, state}}
          end)

        # Here we convert the right side of the `=` into the node in a graph
        # It will be returned as a result of the `left = right` expression
        value_to_id = Map.new(matches, fn {_, value, id} -> {value, id} end)
        {right, id_to_argnum} =
          postwalk(right, %{}, fn tria, id_to_argnum ->
            case value_to_id do
              %{^tria => id} ->
                case id_to_argnum do
                  %{^id => argnum} ->
                    {argnum, id_to_argnum}

                  _ ->
                    argnum = map_size(id_to_argnum)
                    id_to_argnum = Map.put(id_to_argnum, id, argnum)
                    {argnum, id_to_argnum}
                end

              _other ->
                {tria, id_to_argnum}
            end
          end)

        {final_expression_id, graph} = Graph.add_vertex(graph, {:structure_expression, right})
        graph =
          Enum.reduce(id_to_argnum, graph, fn {id, argnum}, graph ->
            Graph.add_edge(graph, final_expression_id, id, {:arg, argnum})
          end)
        state = %{state | graph: graph}

        # Here we create pattern nodes from matches of matchlist
        state =
          Enum.reduce(matches, state, fn {pattern, _value, value_id}, state ->
            pattern
            |> to_matches()
            |> Enum.reduce(state, fn {variable, pattern}, %{graph: graph, vi: vi} = state ->
              case pattern do
                # Tiny optimization to avoid unnecessary transitive node with a `{:pattern, target}`
                {:pattern, v} when is_variable(v) ->
                  vi = Map.put(vi, unmeta(variable), value_id)
                  %{state | vi: vi}

                pattern ->
                  {pattern_id, graph} = Graph.add_vertex(graph, pattern)
                  graph = Graph.add_edge(graph, pattern_id, value_id, {:arg, 0})
                  vi = Map.put(vi, unmeta(variable), pattern_id)
                  %{state | vi: vi, graph: graph}
              end
            end)
          end)

        { final_expression_id, state }

      # Block
      {:__block__, _meta, lines} ->
        Enum.reduce(lines, {start, state}, fn line, {start, state} ->
          pure? = Analyzer.is_pure(line)
          {line_id, state} = translate(line, start, state)
          state =
            if pure? do
              state
            else
              %{state | graph: Graph.add_edge(state.graph, start, line_id, :before)}
            end

          { line_id, state }
        end)

      # Function call
      dot_call(module, function, args) ->
        {args_ids, start, %{graph: graph} = state} = list_translate(args, start, state)
        graph = Graph.add_edge(graph, :start, start, :before)

        arity = length(args)
        {call_id, graph} = Graph.add_vertex(graph, {:call, {module, function, arity}})

        {graph, _} =
          Enum.reduce(args_ids, {graph, 0}, fn arg_id, {graph, argnum} ->
            {Graph.add_edge(graph, call_id, arg_id, {:arg, argnum}), argnum + 1}
          end)

        { call_id, %{state | graph: graph} }

      # Literal
      literal when is_integer(literal) or is_atom(literal) ->
        {id, graph} = Graph.add_vertex(graph, {:literal, literal})
        { id, %{state | graph: graph} }

      # Case
      {:case, _meta, [arg, [do: clauses]]} ->
        {arg_id, state} = translate(arg, start, state)
        {case_id, graph} = Graph.add_vertex(state.graph, {:case, length(clauses)})
        graph = Graph.add_edge(graph, arg_id, case_id, {:arg, 0})
        state = %{state | graph: graph}

        {_counter, state} =
          Enum.reduce(clauses, {0, state}, fn
            {:->, _, [[{:when, _, _}], _body]}, {_counter, _state} ->
              raise "Not implemented"

            {:->, _, [[pattern], body]}, {counter, state} ->
              uniq_var = {:x, [], :erlang.unique_integer([:positive])}
              state = %{state | vi: Map.put(state.vi, uniq_var, arg_id)}
              code = {:__block__, [], [{:=, [], [pattern, uniq_var]}, body]}
              {body_id, state} = translate(code, start, state)

              {clause_id, graph} = Graph.add_vertex(state.graph, {:clause, pattern})
              graph =
                graph
                |> Graph.add_edge(clause_id, body_id, :body)
                |> Graph.add_edge(case_id, clause_id, {:clause, counter})

              state = %{state | graph: graph}
              {counter + 1, state}
          end)

        { case_id, state }

      # # Receive
      # {:receive, meta, [[do: clauses]]} ->
      #   clauses = run_clauses(clauses, translations)
      #   { {:receive, meta, [[do: clauses]]}, translations }

      # {:receive, meta, [[do: clauses, after: {left, right}]]} ->
      #   clauses = run_clauses(clauses, translations)
      #   {left, left_translations} = run(left, translations)
      #   {right, _} = run(right, left_translations)
      #   { {:receive, meta, [[do: clauses, after: {left, right}]]}, translations }

      # # Try
      # {:try, meta, [parts]} ->
      #   parts =
      #     Enum.map(parts, fn
      #       {do_after, body} when do_after in ~w[do after]a ->
      #         {body, _} = run(body, translations)
      #         {do_after, body}

      #       {else_catch, clauses} when else_catch in ~w[else catch]a ->
      #         clauses = run_clauses(clauses, translations)
      #         {else_catch, clauses}
      #     end)

      #   { {:try, meta, [parts]}, translations }

      # # Cond
      # {:cond, meta, [[do: clauses]]} ->
      #   clauses =
      #     Enum.map(clauses, fn {:"->", meta, [left, right]} ->
      #       {left, translations} = run(left, translations)
      #       {right, _} = run(right, translations)
      #       {:"->", meta, [left, right]}
      #     end)
      #   { {:cond, meta, [[do: clauses]]}, translations }

      # # Fn
      # {:fn, meta, clauses} ->
      #   clauses = run_fn_clauses(clauses, translations)
      #   { {:fn, meta, clauses}, translations }

      # # For
      # {:for, meta, iters_opts_body} ->
      #   {opts_body, iters} = List.pop_at(iters_opts_body, -1)
      #   {body, opts} = Keyword.pop!(opts_body, :do)

      #   {iters, inner_translations} = run_left_arrow_clauses(iters, translations)
      #   {opts, inner_translations} = run(opts, inner_translations)

      #   body =
      #     case body do
      #       {:"->", meta, [left, right]} ->
      #         {left, left_translations} = propagate_to_pattern(left, inner_translations)
      #         {right, _right_translations} = run(right, inner_translations <~ left_translations)
      #         {:"->", meta, [left, right]}

      #       body ->
      #         {body, _translations} = run(body, inner_translations)
      #         body
      #     end

      #   { {:for, meta, iters ++ [opts ++ [do: body]]}, translations }

      # # With
      # {:with, meta, clauses} ->
      #   {[{:do, do_clause} | else_clauses?], clauses} = List.pop_at(clauses, -1)
      #   else_clauses = Keyword.get(else_clauses?, :else, [])

      #   {clauses, do_translations} = run_left_arrow_clauses(clauses, translations)
      #   {do_clause, _} = run(do_clause, do_translations)

      #   case run_clauses(else_clauses, translations) do
      #     [] ->
      #       {{:with, meta, clauses ++ [[do: do_clause]]}, translations}

      #     else_clauses ->
      #       {{:with, meta, clauses ++ [[do: do_clause, else: else_clauses]]}, translations}
      #   end

      # # Map cons
      # # TODO think about joining in map cons
      {:"%{}", _map_meta, [{:"|", _cons_meta, [_map, _pairs]}]} ->
        raise "Not supported"
      #   {map, map_translations} = run(map, translations)
      #   {pairs, translations} =
      #     Enum.map_reduce(pairs, map_translations, fn {key, value}, new_translations ->
      #       {key, key_translations} = run(key, translations)
      #       {value, value_translations} = run(value, translations)
      #       {{key, value}, new_translations <~ key_translations <~ value_translations}
      #     end)

      #   { {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]}, translations }

      # Map
      {:"%{}", _meta, pairs} ->
        {structure_expression_pairs, {_start, state}} =
          Enum.map_reduce(pairs, {start, state}, fn {key, value}, {start, state} ->
            pure? = Analyzer.is_pure(key)
            {key_id, %{graph: graph} = state} = translate(key, start, state)
            {start, graph} =
              if pure? do
                {start, graph}
              else
                graph = Graph.add_edge(graph, start, key_id, :before)
                {key_id, graph}
              end
            state = %{state | graph: graph}

            pure? = Analyzer.is_pure(value)
            {value_id, %{graph: graph} = state} = translate(value, start, state)
            {start, graph} =
              if pure? do
                {start, graph}
              else
                graph = Graph.add_edge(graph, start, value_id, :before)
                {value_id, graph}
              end

            {{key_id, value_id}, {start, %{state | graph: graph}}}
          end)

        pairs = Enum.map(0..(length(structure_expression_pairs) - 1), fn i -> {2 * i, 2 * i + 1} end)
        {id, graph} = Graph.add_vertex(state.graph, {:structure_expression, {:%{}, [], pairs}})

        {_, graph} =
          Enum.reduce(structure_expression_pairs, {0, graph}, fn {key_id, value_id}, {count, graph} ->
            graph =
              graph
              |> Graph.add_edge(id, key_id, {:arg, count})
              |> Graph.add_edge(id, value_id, {:arg, count + 1})

            {count + 2, graph}
          end)

        { id, %{state | graph: graph} }

      # # Binary
      # {:"<<>>", meta, items} ->
      #   {items, translationss} =
      #     Enum.map(items, fn
      #       {:"::", meta, [item, type]} ->
      #         {type, _} = run(type, translations)
      #         {item, translations} = run(item, translations)
      #         { {:"::", meta, [item, type]}, translations }

      #       item ->
      #         run(item, translations)
      #     end)
      #     |> Enum.unzip()

      #   { {:"<<>>", meta, items}, merge_translationss(translationss) }

      # Empty list
      [] ->
        {id, graph} = Graph.add_vertex(graph, {:literal, []})
        state = %{state | graph: graph}
        { id, state }

      # List
      items when is_list(items) ->
        {structure_expression, {_start, id_to_argnum, _, %{graph: graph} = state}} =
          Enum.map_reduce(items, {start, %{}, 0, state}, fn
            {:|, _, [left, right]}, {start, id_to_argnum, argnum, state} ->
              {[left_id, right_id], start, state} = list_translate([left, right], start, state)
              id_to_argnum = Map.merge(id_to_argnum, %{left_id => argnum, right_id => argnum + 1})
              {{:|, [], [argnum, argnum + 1]}, {start, id_to_argnum, argnum + 2, state}}

            item, {start, id_to_argnum, argnum, state} ->
              pure? = Analyzer.is_pure(item)
              {id, %{graph: graph} = state} = translate(item, start, state)
              id_to_argnum = Map.put(id_to_argnum, id, argnum)
              {start, graph} =
                if pure? do
                  {start, graph}
                else
                  graph = Graph.add_edge(graph, start, id, :before)
                  {id, graph}
                end

              {argnum, {start, id_to_argnum, argnum + 1, %{state | graph: graph}}}
          end)

        {id, graph} = Graph.add_vertex(graph, {:structure_expression, structure_expression})
        graph =
          Enum.reduce(id_to_argnum, graph, fn {arg_id, argnum}, graph ->
            Graph.add_edge(graph, id, arg_id, {:arg, argnum})
          end)

        state = %{state | graph: graph}
        { id, state }


      # Twople
      {left, right} ->
        {[left_id, right_id], _start, %{graph: graph} = state} = list_translate([left, right], start, state)
        {id, graph} = Graph.add_vertex(graph, {:structure_expression, {0, 1}})
        graph =
          graph
          |> Graph.add_edge(id, left_id, {:arg, 0})
          |> Graph.add_edge(id, right_id, {:arg, 1})

        state = %{state | graph: graph}
        { id, state }

      # Tuple
      {:{}, _, items} ->
        {argids, _start, %{graph: graph} = state} = list_translate(items, start, state)
        {argnums, _} = Enum.map_reduce(argids, 0, fn _, argnum -> {argnum, argnum + 1} end)
        {id, graph} = Graph.add_vertex(graph, {:structure_expression, {:{}, [], argnums}})
        {graph, _} =
          Enum.reduce(argids, {graph, 0}, fn argid, {graph, argnum} ->
            {Graph.add_edge(graph, id, argid, {:arg, argnum}), argnum + 1}
          end)

        state = %{state | graph: graph}
        { id, state }

      # # Dot
      # {:., dotmeta, dot} ->
      #   {dot, dot_translations} = run(dot, translations)
      #   { {:., dotmeta, dot}, dot_translations }

      # # Calls and forms
    end
  end

  # Translates proper list, which can be found in function args, proper list or a long tuple
  defp list_translate(items, start, state) do
    {ids, {start, state}} =
      Enum.map_reduce(items, {start, state}, fn item, {start, state} ->
        pure? = Analyzer.is_pure(item)
        {id, %{graph: graph} = state} = translate(item, start, state)
        {start, graph} =
          if pure? do
            {start, graph}
          else
            graph = Graph.add_edge(graph, start, id, :before)
            {id, graph}
          end

        {id, {start, %{state | graph: graph}}}
      end)

    {ids, start, state}
  end

  def deduplicate(state) do
    state.graph.i
    |> Enum.group_by(fn {_key, value} -> value end, fn {key, _value} -> key end)
    |> Enum.each(fn {_, values} -> if length(values) > 1, do: IO.inspect(values) end)

    state
  end

  def cleanup(state) do
    i =
      Enum.reduce(state.graph.i, state.graph.i, fn {id, {value, links}}, i ->
        case value do
          {:structure_expression, 0} ->
             i = Map.delete(i, id)
             replacement = Map.fetch!(links, {:arg, 0})
             Enum.reduce(links, i, fn
               {{:back, link}, id}, i ->
                 Map.update!(i, id, fn {value, links} ->
                   {value, Map.replace!(links, link, replacement)}
                 end)

               _, i ->
                 i
             end)

          _ ->
            i
        end
      end)

    %{state | graph: %{state.graph | i: i}}
  end
end
