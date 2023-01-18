defmodule Tria.Compiler.SSATranslator do

  @moduledoc """
  SSA form is a subset of Tria language.
  """

  @behaviour Tria.Compiler.Translator

  import Tria.Language
  import Tria.Language.Meta

  defstruct [
    pin_known:    false,
    translations: %{}
  ]

  @typedoc """
  - `:pin_known` -- For translating from Erlang, since it has no pins for variables
  and does not support shadowing for anything except arguments in `fun`s
  """
  @type option :: {:pin_known, false}

  # Public

  @doc """
  Because Tria.SSA is a subset of Tria
  no translation is required
  """
  def to_tria(ast, _opts \\ []) do
    ast
  end

  @doc """
  Creates Single static assignment form of Tria language
  """
  def from_tria!(ast, opts \\ []) do
    {ssa_form_ast, _} = from_tria(ast, opts)
    ssa_form_ast
  rescue
    x ->
      inspect_ast(ast, label: :failed)
      reraise x, __STACKTRACE__
  end

  @doc """
  Creates Single static assignment form of Tria language and
  also returns a state of SSATranslator after traversal
  """
  def from_tria(ast, opts \\ [])
  def from_tria([{:"->", _, _}] = clauses, opts) do
    with {{:fn, _, clauses}, translations} <- from_tria({:fn, [], clauses}, opts) do
      {clauses, translations}
    end
  end
  def from_tria(ast, opts) do
    run(ast, %__MODULE__{pin_known: Keyword.get(opts, :pin_known, false)})
  end

  # Main recursive function
  defp run(code, translations) do
    case code do
      # Variable
      {_, meta, _} = variable when is_variable(variable) ->
        case fetch_translation(translations, variable) do
          {:ok, replacement} ->
            {with_meta(replacement, meta), translations}

          :error ->
            # It appears that the variableiable is undefined
            # And we just leave it be
            {variable, translations}
        end

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
      {:case, meta, [arg, [do: clauses]]} ->
        {arg, translations} = run(arg, translations)
        clauses = run_clauses(clauses, translations)
        { {:case, meta, [arg, [{:do, clauses}]]}, translations }

      # Receive
      {:receive, meta, [[do: clauses]]} ->
        clauses = run_clauses(clauses, translations)
        { {:receive, meta, [[do: clauses]]}, translations }

      {:receive, meta, [[do: clauses, after: {left, right}]]} ->
        clauses = run_clauses(clauses, translations)
        {left, left_translations} = run(left, translations)
        {right, _} = run(right, left_translations)
        { {:receive, meta, [[do: clauses, after: {left, right}]]}, translations }

      # Try
      {:try, meta, [parts]} ->
        parts =
          Enum.map(parts, fn
            {do_after, body} when do_after in ~w[do after]a ->
              {body, _} = run(body, translations)
              {do_after, body}

            {else_catch, clauses} when else_catch in ~w[else catch]a ->
              clauses = run_clauses(clauses, translations)
              {else_catch, clauses}
          end)

        { {:try, meta, [parts]}, translations }

      # Cond
      {:cond, meta, [[do: clauses]]} ->
        clauses =
          Enum.map(clauses, fn {:"->", meta, [left, right]} ->
            {left, translations} = run(left, translations)
            {right, _} = run(right, translations)
            {:"->", meta, [left, right]}
          end)
        { {:cond, meta, [[do: clauses]]}, translations }

      # Fn
      {:fn, meta, clauses} ->
        clauses = run_fn_clauses(clauses, translations)
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
      {:"%{}", meta, pairs} ->
        {pairs, translations} =
          Enum.map_reduce(pairs, translations, fn {key, value}, new_translations ->
            {key, key_translations} = run(key, translations)
            {value, value_translations} = run(value, translations)
            {{key, value}, new_translations <~ key_translations <~ value_translations}
          end)

        { {:"%{}", meta, pairs}, translations }

      # Binary
      {:"<<>>", meta, items} ->
        {items, translationss} =
          Enum.map(items, fn
            {:"::", meta, [item, type]} ->
              {type, _} = run(type, translations)
              {item, translations} = run(item, translations)
              { {:"::", meta, [item, type]}, translations }

            item ->
              run(item, translations)
          end)
          |> Enum.unzip()

        { {:"<<>>", meta, items}, merge_translationss(translationss) }

      # Empty list
      [] ->
        { [], translations }

      # List
      items when is_list(items) ->
        if List.improper?(items) do
          inspect_ast(items, label: :improper_list)
        end

        {items, translationss} =
          items
          |> Enum.map(fn item -> run(item, translations) end)
          |> Enum.unzip()

        { items, merge_translationss(translationss) }

      # Twople
      {left, right} ->
        {left, left_translations} = run(left, translations)
        {right, right_translations} = run(right, translations)
        { {left, right}, left_translations <~ right_translations }

      # Tuple
      {:{}, meta, items} ->
        {items, translations} = run(items, translations)
        { {:{}, meta, items}, translations }

      # Dot
      {:., dotmeta, dot} ->
        {dot, dot_translations} = run(dot, translations)
        { {:., dotmeta, dot}, dot_translations }

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
        { other, translations }
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
        run(other, translations)
    end)
  end

  defp run_clauses(clauses, translations) do
    Enum.map(clauses, fn {:"->", meta, [left, right]} ->
      {left, left_translations} = propagate_to_pattern(left, translations)
      {right, _} = run(right, translations <~ left_translations)
      {:"->", meta, [left, right]}
    end)
  end

  defp run_fn_clauses(clauses, translations) do
    Enum.map(clauses, fn {:"->", meta, [left, right]} ->
      {left, left_translations} = propagate_to_pattern(left, %__MODULE__{translations | pin_known: false})
      {right, _} = run(right, translations <~ left_translations)
      {:"->", meta, [left, right]}
    end)
  end

  ## Propagating to pattern

  def propagate_to_pattern(code, translations, new_translations \\ %__MODULE__{}) do
    case code do
      # Variable
      {:_, _, _} = underscore when is_variable(underscore) ->
        # We SSA underscores to avoid situations where code analyzer doesn't know about underscores
        {unify(underscore), new_translations}

      {_, meta, _} = variable when is_variable(variable) ->
        {new_variable, new_new_translations} =
          case fetch_translation(new_translations, variable) do
            :error ->
              # It appears that it is the new variable
              new_variable = unify(variable)
              {new_variable, put_translation(new_translations, variable, new_variable)}

            {:ok, variable} ->
              # It appears that the variable is present multiple times in the pattern
              {with_meta(variable, meta), new_translations}
          end

        key = unmeta variable
        case translations do
          %__MODULE__{pin_known: true, translations: %{^key => value}} ->
            pinned = pin with_meta(value, meta)
            {pinned, new_translations}

          _ ->
            {new_variable, new_new_translations}
        end

      # Binary matching is special because pinning is not required for variables in types
      {:"<<>>", meta, items} ->
        {items, new_translations} =
          Enum.map_reduce(items, new_translations, fn
            {:"::", meta, [item, type]}, new_translations ->
              # First we just put variables into the type part of the binary match
              {type, _} = run(type, translations <~ new_translations)

              # And then we get definitions from left side of ::
              {item, new_translations} = propagate_to_pattern(item, translations, new_translations)

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
      pin(variable, meta) = pinned when is_variable(variable) ->
        case fetch_translation(translations, variable) do
          :error ->
            {pinned, new_translations}

          {:ok, val} ->
            {pin(val, meta), new_translations}
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

  defp merge_translationss([]), do: %__MODULE__{}
  defp merge_translationss(translationss) when is_list(translationss) do
    Enum.reduce(translationss, fn right, left -> left <~ right end)
  end

  defp %__MODULE__{pin_known: lpin, translations: left} <~ %__MODULE__{pin_known: rpin, translations: right} do
    %__MODULE__{translations: Map.merge(left, right), pin_known: lpin or rpin}
  end

  defp put_translation(%{translations: translations} = state, key, value) do
    key = unmeta key
    value = unmeta value
    %{state | translations: Map.put(translations, key, value)}
  end

  defp fetch_translation(%{translations: translations} = state, key) do
    key = unmeta key
    case translations do
      %{^key => value} ->
        with :error <- fetch_translation(state, value) do
          {:ok, value}
        end

      _ ->
        :error
    end
  end

  ## Helpers for variables

  defp unify({varname, meta, _context}) do
    {varname, meta, gen_uniq_context()}
  end

end
