defmodule Tria.Translator.SSA do

  @moduledoc """
  SSA form is a subset of Tria language
  """

  @behaviour Tria.Translator

  import Tria.Common
  import Tria.Tri

  # Public

  def to_tria(ast, _opts \\ []) do
    ast
  end

  def from_tria(ast) do
    {ssa_form_ast, _} = run(ast, %{})
    ssa_form_ast
  end

  # Main recursive function
  defp run(code, translations) do
    case code do
      # Block
      {:__block__, meta, lines} ->
        {lines, translations} = Enum.map_reduce(lines, translations, &run/2)
        { {:__block__, meta, lines}, translations }

      # Equals
      {:=, meta, [left, right]} ->
        {left, left_translations} = propagate_to_pattern(left, translations)
        {right, right_translations} = run(right, translations)
        { {:=, meta, [left, right]}, right_translations <~ left_translations }

      # Case
      {:case, meta, [arg, [{:do, clauses}]]} ->
        {arg, translations} = run(arg, translations)
        clauses = run_clauses(clauses, translations)
        { {:case, meta, [arg, [{:do, clauses}]]}, translations }

      # Fn
      {:fn, meta, clauses} ->
        clauses = run_clauses(clauses, translations)
        { {:fn, meta, clauses}, translations }

      # For
      {:for, meta, iters_opts_body} ->
        {opts_body, iters} = List.pop_at(iters_opts_body, -1)
        {body, opts} = Keyword.pop!(opts_body, :do)

        {iters, inner_translations} = run_left_arrow_clauses(iters, translations)
        {opts, inner_translations} = run(opts, inner_translations)

        body =
          case body do
            {:"->", meta, [left, right]} ->
              {left, left_translations} = propagate_to_pattern(left, inner_translations)
              {right, _right_translations} = run(right, inner_translations <~ left_translations)
              {:"->", meta, [left, right]}

            body ->
              {body, _translations} = run(body, inner_translations)
              body
          end

        { {:for, meta, iters ++ [opts ++ [do: body]]}, translations }

      # With
      {:with, meta, clauses} ->
        {[{:do, do_clause} | else_clauses?], clauses} = List.pop_at(clauses, -1)
        else_clauses = Keyword.get(else_clauses?, :else, [])

        {clauses, do_translations} = run_left_arrow_clauses(clauses, translations)
        {do_clause, _} = run(do_clause, do_translations)

        case run_clauses(else_clauses, translations) do
          [] ->
            {{:with, meta, clauses ++ [[do: do_clause]]}, translations}

          else_clauses ->
            {{:with, meta, clauses ++ [[do: do_clause, else: else_clauses]]}, translations}
        end

      # Map cons
      # TODO think about joining in map cons
      {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]} ->
        {map, map_translations} = run(map, translations)
        {pairs, translations} =
          Enum.map_reduce(pairs, map_translations, fn {key, value}, new_translations ->
            {key, key_translations} = run(key, translations)
            {value, value_translations} = run(value, translations)
            {{key, value}, new_translations <~ key_translations <~ value_translations}
          end)

        { {:"%{}", map_meta, [{:"|", cons_meta, [map, pairs]}]}, translations }

      # Map
      tri %{tri_splicing pairs} ->
        {pairs, translations} =
          Enum.map_reduce(pairs, translations, fn {key, value}, new_translations ->
            {key, key_translations} = run(key, translations)
            {value, value_translations} = run(value, translations)
            {{key, value}, new_translations <~ key_translations <~ value_translations}
          end)

        { quote(do: %{unquote_splicing pairs}), translations }

      # Binary
      {:"<<>>", meta, items} ->
        {items, translationss} =
          Enum.map(items, fn
            {:"::", meta, [item, type]} ->
              {item, new_translations} = run(item, translations)
              {type, _type_translations} = run(type, translations)
              {{:"::", meta, [item, type]}, new_translations}

            item ->
              run(item, translations)
          end)
          |> Enum.unzip()

        {{:"<<>>", meta, items}, merge_translationss(translationss)}

      # Empty list
      [] ->
        {[], translations}

      # List
      items when is_list(items) ->
        {items, translationss} =
          items
          |> Enum.map(fn item -> run(item, translations) end)
          |> Enum.unzip()

        {items, merge_translationss(translationss)}

      # Twople
      {left, right} ->
        {left, left_translations} = run(left, translations)
        {right, right_translations} = run(right, translations)
        {{left, right}, left_translations <~ right_translations}

      # Tuple
      tri {tri_splicing items} ->
        {items, translations} = run(items, translations)
        {quote(do: {unquote_splicing(items)}), translations}

      # Dot
      {:., dotmeta, dot} ->
        {dot, dot_translations} = run(dot, translations)
        { {:., dotmeta, dot}, dot_translations }

      # Variable
      var when is_variable(var) ->
        case fetch_translation(translations, var) do
          {:ok, value} ->
            {value, translations}

          :error ->
            # It appears that the variable is undefined
            # And we just leave it be
            {var, translations}
        end

      # Calls and forms
      {caller, meta, args} ->
        {caller, caller_translations} = run(caller, translations)
        {args, args_translations} = run(args, translations)

        {
          {caller, meta, args},
          caller_translations <~ args_translations
        }

      # Literal
      other ->
        {other, translations}
    end
  end

  ## Clauses helpers

  defp run_left_arrow_clauses(clauses, translations) do
    Enum.map_reduce(clauses, translations, fn
      {:"<-", meta, [left, right]}, translations ->
        {left, left_translations} = propagate_to_pattern(left, translations)
        {right, _right_translations} = run(right, translations)
        { {:"<-", meta, [left, right]}, translations <~ left_translations }

      other, translations ->
        {other, _translations} = run(other, translations)
        {other, translations}
    end)
  end

  defp run_clauses(clauses, translations) do
    Enum.map(clauses, fn {:"->", meta, [left, right]} ->
      {left, left_translations} = propagate_to_pattern(left, translations)
      {right, _} = run(right, translations <~ left_translations)
      {:"->", meta, [left, right]}
    end)
  end

  ## Propagating to pattern

  def propagate_to_pattern(code, translations, new_translations \\ %{}) do
    case code do
      # Binary matching is special because pinning is not required for variables in types
      {:"<<>>", meta, items} ->
        {items, new_translations} =
          Enum.map_reduce(items, new_translations, fn
            {:"::", meta, [item, type]}, new_translations ->
              {item, new_translations} = propagate_to_pattern(item, translations, new_translations)

              # Here we just put variables into the type part of the binary match
              {type, _} = run(type, translations)
              {{:"::", meta, [item, type]}, new_translations}

            item, new_translations ->
              propagate_to_pattern(item, translations, new_translations)
          end)

        {{:"<<>>", meta, items}, new_translations}

      # When
      {:when, meta, pattern_and_guard} ->
        {guard, patterns} = List.pop_at(pattern_and_guard, -1)
        {patterns, pattern_translations} = propagate_to_pattern(patterns, translations, new_translations)

        # Since guard is not creating new variables, we can just run evaluation against it
        {guard, _guard_translations} = run(guard, translations <~ pattern_translations)

        {{:when, meta, patterns ++ [guard]}, pattern_translations}

      # Pin . We handle pin separately before variables
      {:^, meta, [variable]} = pinned when is_variable(variable) ->
        case fetch_translation(translations, variable) do
          :error ->
            {pinned, new_translations}

          {:ok, val} ->
            {{:^, meta, [val]}, new_translations}
        end

      # Variable
      variable when is_variable(variable) ->
        case fetch_translation(new_translations, variable) do
          :error ->
            # It appears that it is the new variable
            new_variable = unify(variable)
            {new_variable, put_translation(new_translations, variable, new_variable)}

          {:ok, variable} ->
            # It appears that the variable is present multiple times in the pattern
            {variable, new_translations}
        end

      # Map or Tuple or Binary or matchable operator <>
      {n, m, items} when is_list(items) ->
        {items, new_translations} = propagate_to_pattern(items, translations, new_translations)
        {{n, m, items}, new_translations}

      # List
      items when is_list(items) ->
        Enum.map_reduce(items, new_translations, fn item, acc_translations ->
          propagate_to_pattern(item, translations, acc_translations)
        end)

      # Twople
      {left, right} ->
        {left, new_translations} = propagate_to_pattern(left, translations, new_translations)
        {right, new_translations} = propagate_to_pattern(right, translations, new_translations)

        {{left, right}, new_translations}
  
      # Literal
      literal when is_literal(literal) ->
        {literal, new_translations}
    end
  end

  ## Helpers for working with translations

  defp merge_translationss([]), do: %{}
  defp merge_translationss(translationss) when is_list(translationss) do
    Enum.reduce(translationss, fn right, left -> left <~ right end)
  end

  defp left <~ right do
    Map.merge(left, right)
  end

  defp put_translation(translations, key, value) do
    key = unmeta key
    value = unmeta value
    Map.put(translations, key, value)
  end

  defp fetch_translation(translations, key) do
    key = unmeta key
    case translations do
      %{^key => value} ->
        with :error <- fetch_translation(translations, value) do
          {:ok, value}
        end

      _ ->
        :error
    end
  end

  defp unify({varname, meta, _cotnext}) do
    {varname, meta, gen_uniq_context()}
  end

  defp unmeta({name, meta, context}) when is_variable(name, meta, context) do
    {name, [], context}
  end

end
