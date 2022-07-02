defmodule Tria.Pass.Evaluation do
  @moduledoc """
  Pass which evaluates, propagates and drops dead code in one run
  """

  import Tria.Common
  import Tria.Matcher
  alias Tria.Interpreter

  defp (%{bindings: left_bindings} = left) <~ (%{bindings: right_bindings}) do
    %{left | bindings: Map.merge(left_bindings, right_bindings)}
  end

  defstruct [
    bindings: %{},
    evaluate: %{}
  ]

  def run_once(ast, configuration) do
    do_run(ast, configuration)
  end

  # Block
  defp do_run({:__block__, meta, block}, configuration) do
    {reversed_lines, new_configuration} =
      Enum.reduce(block, {[], configuration}, fn line, {lines, configuration} ->
        {line, new_configuration} = do_run(line, configuration)
        {[line | lines], new_configuration}
      end)

    {
      {:__block__, meta, Enum.reverse(reversed_lines)},
      new_configuration
    }
  end

  # Equals
  defp do_run(tri(left = right), configuration) do
    {right, right_configuration} = do_run(right, configuration)
    {left, left_configuration} = propagate_to_pattern(left, configuration)

    if Macro.quoted_literal?(right) do
      {:yes, bindings} = Interpreter.match?(left, right)
      bindings =
        Enum.map(bindings, fn {{name, _, context} = v, value} when is_variable(v) ->
          {{name, context}, value}
        end)

      {
        right,
        configuration <~ %__MODULE__{bindings: bindings}
      }
    else
      {
        quote(do: unquote(left) = unquote(right)),
        configuration <~ right_configuration <~ left_configuration
      }
    end
  end

  # Case
  defp do_run(tri(case(arg, do: clauses)), configuration) do
    {arg, configuration} = do_run(arg, configuration)

    clauses =
      Enum.map(clauses, fn {:->, meta, [left, right]} ->
        {left, left_configuration} = propagate_to_pattern(left, configuration)
        {right, _right_configuration} = do_run(right, configuration <~ left_configuration)
        {:->, meta, [left, right]}
      end)

    {quote(do: case(unquote(arg), do: unquote(clauses))), configuration}
  end

  # For
  defp do_run({:for, _meta, _iters_and_stuff}, _configuration) do
    raise "Not implemented yet"
  end

  # If
  defp do_run({:if, _, [arg, {:do, do_clause} | maybe_else]}, configuration) do
    else_clause = Keyword.get(maybe_else, :else)
    {arg, configuration} = do_run(arg, configuration)
    {do_clause, _} = do_run(do_clause, configuration)
    {else_clause, _} = do_run(else_clause, configuration)

    {
      quote(do: if(unquote(arg), do: unquote(do_clause), else: unquote(else_clause))),
      configuration
    }
  end

  # With
  defp do_run({:with, meta, clauses}, configuration) do
    {do_clause, clauses} = Keyword.pop!(clauses, :do)
    {else_clauses, clauses} = Keyword.pop(clauses, :else)

    {reversed_clauses, do_configuration} =
      Enum.reduce(clauses, {[], configuration}, fn
        {op, meta, [left, right]}, {clauses, configuration} ->
          {right, _} = do_run(right, configuration)
          {left, left_configuration} = propagate_to_pattern(configuration, left)

          {
            [{op, meta, [left, right]} | clauses],
            configuration <~ left_configuration
          }
      end)

    clauses = Enum.reverse(reversed_clauses)
    {do_clause, _} = do_run(do_clause, do_configuration)

    {tri(case(_, do: else_clauses)), _} =
      do_run(quote(do: case(nil, do: unquote(else_clauses))), configuration)

    {{:with, meta, clauses ++ [do: do_clause, else: else_clauses]}, configuration}
  end

  # Structs
  defp do_run(tri(%module{tri_splicing(pairs)}), configuration) do
    {tri(%{tri_splicing(pairs)}), configuration} = do_run(quote(do: %{unquote_splicing(pairs)}), configuration)

    {
      quote(do: %unquote(module){unquote_splicing(pairs)}),
      configuration
    }
  end

  # Maps
  defp do_run(tri(%{tri_splicing(pairs)}), configuration) do
    pairs_with_configuration =
      Enum.map(pairs, fn {key, value} ->
        {do_run(key, configuration), do_run(value, configuration)}
      end)

    pairs = Enum.map(pairs_with_configuration, fn {{key, _}, {value, _}} -> {key, value} end)

    configuration =
      Enum.reduce(pairs_with_configuration, configuration, fn {{_, key}, {_, value}}, configuration ->
        configuration <~ key <~ value
      end)

    {
      quote(do: %{unquote_splicing(pairs)}),
      configuration
    }
  end

  # Binary
  defp do_run(tri(<<tri_splicing(items)>>), configuration) do
    {items, configurations} =
      Enum.map(items, fn
        # TODO find out if the propagation is required for the right side
        tri(item :: type) ->
          {item, new_configuration} = do_run(item, configuration)
          {quote(do: unquote(item) :: unquote(type)), new_configuration}

        item ->
          do_run(item, configuration)
      end)
      |> Enum.unzip()

    {quote(do: <<unquote_splicing(items)>>), merge_configuration(configurations)}
  end

  # List
  defp do_run(items, configuration) when is_list(items) do
    {items, configurations} =
      items
      |> Enum.map(fn item -> do_run(item, configuration) end)
      |> Enum.unzip()

    {items, merge_configuration(configurations)}
  end

  # Tuple
  defp do_run({left, right}, configuration) do
    {left, left_configuration} = do_run(left, configuration)
    {right, right_configuration} = do_run(right, configuration)
    {{left, right}, left_configuration <~ right_configuration}
  end

  defp do_run(tri({tri_splicing(items)}), configuration) do
    {items, configuration} = do_run(items, configuration)
    {quote(do: {unquote_splicing(items)}), configuration}
  end

  # Dot
  defp do_run({{:., dotmeta, dot}, meta, args}, configuration) do
    {dot, dot_configuration} = do_run(dot, configuration)
    {args, args_configuration} = do_run(args, configuration)

    {
      {{:., dotmeta, dot}, meta, args},
      dot_configuration <~ args_configuration
    }
  end

  # Function call
  defp do_run({func, meta, args}, configuration) when is_list(args) do
    {args, args_configuration} = do_run(args, configuration)

    {
      {func, meta, args},
      args_configuration
    }
  end

  # Variable
  defp do_run(var, configuration) when is_variable(var) do
    case get_bind(configuration, var) do
      nil -> {var, configuration}
      value -> {value, configuration}
    end
  end

  # Literal
  defp do_run(other, configuration), do: {other, configuration}

  # Helpers

  # Double `s` in configurations is intentional
  defp merge_configuration(configurations) when is_list(configurations) do
    Enum.reduce(configurations, fn right, left -> left <~ right end)
  end

  # Propagating to pattern

  defp propagate_to_pattern({:^, _, [variable]} = pinned, configuration) when is_variable(variable) do
    case get_bind(configuration, variable) do
      nil -> {pinned, %{}}
      val -> {val, %{}}
    end
  end

  defp propagate_to_pattern(variable, _configuration) when is_variable(variable) do
    new_variable = unify(variable)
    {new_variable, put_bind(%__MODULE__{}, variable, new_variable)}
  end

  # Call
  defp propagate_to_pattern({n, m, items}, configuration) when is_list(items) do
    {items, new_configuration} = propagate_to_pattern(items, configuration)
    {{n, m, items}, new_configuration}
  end

  # List
  defp propagate_to_pattern(items, configuration) when is_list(items) do
    {reversed_items, new_configuration} =
      Enum.reduce(items, {[], %{}}, fn item, {items, acc_configuration} ->
        {item, new_configuration} = propagate_to_pattern(item, configuration)
        {[item | items], acc_configuration <~ new_configuration}
      end)

    {Enum.reverse(reversed_items), new_configuration}
  end

  # Twople
  defp propagate_to_pattern({left, right}, configuration) do
    {left, left_configuration} = propagate_to_pattern(left, configuration)
    {right, right_configuration} = propagate_to_pattern(right, configuration)

    {
      {left, right},
      left_configuration <~ right_configuration
    }
  end
  
  # Literal
  defp propagate_to_pattern(other, _configuration), do: {other, %{}}

  # Helpers

  defp get_bind(%{bindings: bindings}, {name, _, context}) do
    with {bound_name, bound_context} <- Map.get(bindings, {name, context}) do
      {bound_name, [], bound_context}
    end
  end

  defp put_bind(%{bindings: bindings} = cfg, {name, _, context}, {bind_to_name, _, bind_to_context}) do
    %{cfg | bindings: Map.put(bindings, {name, context}, {bind_to_name, bind_to_context})}
  end

  defp unify({varname, meta, _cotnext}) do
    {varname, meta, gen_uniq_context()}
  end
end
