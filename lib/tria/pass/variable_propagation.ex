defmodule Tria.Pass.VariablePropagation do
  @moduledoc """
  Pass for propagating variables from bindings into AST
  It also isolates variables newly defined in this context
  into another context

  #TODO: for
  #TODO: with edge cases
  #TODO: test contexts intersection
  """

  import Tria.Common
  import Tria.Tri

  defmacrop l <~ r, do: quote(do: :maps.merge(unquote(l), unquote(r)))

  def run_once(ast, bindings \\ %{})
  def run_once(ast, bindings) when is_map(bindings), do: do_run(ast, bindings)
  def run_once(ast, bindings) do
    {ast, bindings} = do_run(ast, Map.new(bindings))
    {ast, Map.to_list(bindings)}
  end

  # Block
  defp do_run({:__block__, meta, block}, bindings) do
    {reversed_lines, new_bindings} =
      Enum.reduce(block, {[], bindings}, fn line, {lines, bindings} ->
        {line, new_bindings} = do_run(line, bindings)
        {[line | lines], new_bindings}
      end)

    {
      {:__block__, meta, Enum.reverse(reversed_lines)},
      new_bindings
    }
  end

  # Equals
  defp do_run(tri(left = right), bindings) do
    {right, right_bindings} = do_run(right, bindings)
    {left, left_bindings} = propagate_to_pattern(left, bindings)

    {
      quote(do: unquote(left) = unquote(right)),
      bindings <~ right_bindings <~ left_bindings
    }
  end

  # Case
  defp do_run(tri(case(arg, do: clauses)), bindings) do
    {arg, bindings} = do_run(arg, bindings)

    clauses =
      Enum.map(clauses, fn {:->, meta, [left, right]} ->
        {left, left_bindings} = propagate_to_pattern(left, bindings)
        {right, _right_bindings} = do_run(right, bindings <~ left_bindings)
        {:->, meta, [left, right]}
      end)

    {quote(do: case(unquote(arg), do: unquote(clauses))), bindings}
  end

  # For
  defp do_run({:for, _meta, _iters_and_stuff}, _bindings) do
    raise "Not implemented yet"
  end

  # With
  defp do_run({:with, meta, clauses}, bindings) do
    {do_clause, clauses} = Keyword.pop!(clauses, :do)
    {else_clauses, clauses} = Keyword.pop(clauses, :else)

    {reversed_clauses, do_bindings} =
      Enum.reduce(clauses, {[], bindings}, fn
        {op, meta, [left, right]}, {clauses, bindings} ->
          {right, _} = do_run(right, bindings)
          {left, left_bindings} = propagate_to_pattern(bindings, left)

          {
            [{op, meta, [left, right]} | clauses],
            bindings <~ left_bindings
          }
      end)

    clauses = Enum.reverse(reversed_clauses)
    {do_clause, _} = do_run(do_clause, do_bindings)

    {tri(case(_, do: else_clauses)), _} =
      do_run(quote(do: case(nil, do: unquote(else_clauses))), bindings)

    {{:with, meta, clauses ++ [do: do_clause, else: else_clauses]}, bindings}
  end

  # Structs
  defp do_run(tri(%module{tri_splicing(pairs)}), bindings) do
    {tri(%{tri_splicing(pairs)}), bindings} = do_run(quote(do: %{unquote_splicing(pairs)}), bindings)

    {
      quote(do: %unquote(module){unquote_splicing(pairs)}),
      bindings
    }
  end

  # Maps
  defp do_run(tri(%{tri_splicing(pairs)}), bindings) do
    pairs_with_bindings =
      Enum.map(pairs, fn {key, value} ->
        {do_run(key, bindings), do_run(value, bindings)}
      end)

    pairs = Enum.map(pairs_with_bindings, fn {{key, _}, {value, _}} -> {key, value} end)

    bindings =
      Enum.reduce(pairs_with_bindings, bindings, fn {{_, key}, {_, value}}, bindings ->
        bindings <~ key <~ value
      end)

    {
      quote(do: %{unquote_splicing(pairs)}),
      bindings
    }
  end

  # Binary
  defp do_run(tri(<<tri_splicing(items)>>), bindings) do
    {items, bindingss} =
      Enum.map(items, fn
        # TODO find out if the propagation is required for the right side
        tri(item :: type) ->
          {item, new_bindings} = do_run(item, bindings)
          {quote(do: unquote(item) :: unquote(type)), new_bindings}

        item ->
          do_run(item, bindings)
      end)
      |> Enum.unzip()

    {quote(do: <<unquote_splicing(items)>>), merge_bindings(bindingss)}
  end

  # List
  defp do_run(items, bindings) when is_list(items) do
    {items, bindingss} =
      items
      |> Enum.map(fn item -> do_run(item, bindings) end)
      |> Enum.unzip()

    {items, merge_bindings(bindingss)}
  end

  # Tuple
  defp do_run({left, right}, bindings) do
    {left, left_bindings} = do_run(left, bindings)
    {right, right_bindings} = do_run(right, bindings)
    {{left, right}, left_bindings <~ right_bindings}
  end

  defp do_run(tri({tri_splicing(items)}), bindings) do
    {items, bindings} = do_run(items, bindings)
    {quote(do: {unquote_splicing(items)}), bindings}
  end

  # Dot
  defp do_run({{:., dotmeta, dot}, meta, args}, bindings) do
    {dot, dot_bindings} = do_run(dot, bindings)
    {args, args_bindings} = do_run(args, bindings)

    {
      {{:., dotmeta, dot}, meta, args},
      dot_bindings <~ args_bindings
    }
  end

  # Function call
  defp do_run({func, meta, args}, bindings) when is_list(args) do
    {args, args_bindings} = do_run(args, bindings)

    {
      {func, meta, args},
      args_bindings
    }
  end

  # Variable
  defp do_run(var, bindings) when is_variable(var) do
    case get_bind(bindings, var) do
      nil -> {var, bindings}
      value -> {value, bindings}
    end
  end

  # Literal
  defp do_run(other, bindings), do: {other, bindings}

  # Helpers

  # Double `s` in bindingss is intentional
  defp merge_bindings(bindingss) when is_list(bindingss) do
    Enum.reduce(bindingss, fn right, left -> left <~ right end)
  end

  # Propagating to pattern

  defp propagate_to_pattern({:^, _, [variable]} = pinned, bindings) when is_variable(variable) do
    case get_bind(bindings, variable) do
      nil -> {pinned, %{}}
      val -> {val, %{}}
    end
  end

  defp propagate_to_pattern(variable, _bindings) when is_variable(variable) do
    new_variable = unify(variable)
    {new_variable, put_bind(%{}, variable, new_variable)}
  end

  # Call
  defp propagate_to_pattern({n, m, items}, bindings) when is_list(items) do
    {items, new_bindings} = propagate_to_pattern(items, bindings)
    {{n, m, items}, new_bindings}
  end

  # List
  defp propagate_to_pattern(items, bindings) when is_list(items) do
    {reversed_items, new_bindings} =
      Enum.reduce(items, {[], %{}}, fn item, {items, acc_bindings} ->
        {item, new_bindings} = propagate_to_pattern(item, bindings)
        {[item | items], acc_bindings <~ new_bindings}
      end)

    {Enum.reverse(reversed_items), new_bindings}
  end

  # Twople
  defp propagate_to_pattern({left, right}, bindings) do
    {left, left_bindings} = propagate_to_pattern(left, bindings)
    {right, right_bindings} = propagate_to_pattern(right, bindings)

    {
      {left, right},
      left_bindings <~ right_bindings
    }
  end
  
  # Literal
  defp propagate_to_pattern(other, _bindings), do: {other, %{}}

  # Helpers

  defp get_bind(bindings, {name, _, context}) do
    with {bound_name, bound_context} <- Map.get(bindings, {name, context}) do
      {bound_name, [], bound_context}
    end
  end

  defp put_bind(bindings, {name, _, context}, {bind_to_name, _, bind_to_context}) do
    Map.put(bindings, {name, context}, {bind_to_name, bind_to_context})
  end

  defp unify({varname, meta, _cotnext}) do
    {varname, meta, gen_uniq_context()}
  end
end
