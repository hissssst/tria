defmodule Tria.Pass.Evaluation do
  @moduledoc """
  Pass which evaluates, propagates and drops dead code in one run
  This pass performs almost no analysis and just tries to preevaluate any
  forms it comes accross
  """

  import Tria.Common
  import Tria.Tri
  alias Tria.Interpreter

  defstruct [
    bindings: %{},
    pure_functions: []
  ]

  def run_once!(ast, configuration \\ [])
  def run_once!(ast, configuration) when is_list(configuration) do
    run_once!(ast, struct!(__MODULE__, configuration))
  end
  def run_once!(ast, configuration) do
    {code, _bindings} = do_run(ast, configuration)
    code
  end

  # Block
  defp do_run({:__block__, meta, block}, configuration) do
    {reversed_lines, new_configuration} =
      Enum.reduce(block, {[], configuration}, fn line, {lines, configuration} ->
        {line, new_configuration} = do_run(line, configuration)
        {[line | lines], new_configuration}
      end)

    block =
      configuration
      |> drop_literals_in_block(Enum.reverse reversed_lines)
      |> case do
        [] -> raise "Somebody fucked up"
        [line] -> line
        lines -> {:__block__, meta, lines}
      end

    {
      block,
      new_configuration
    }
  end

  # Equals
  defp do_run(tri(left = right), configuration) do
    {right, right_configuration} = do_run(right, configuration)
    {left, left_configuration} = propagate_to_pattern(left, configuration)

    if Macro.quoted_literal?(right)  do
      {:yes, bindings} = Interpreter.match?(left, right)
      bindings =
        Map.new(bindings, fn {{name, _, context} = v, value} when is_variable(v) ->
          {{name, context}, value}
        end)

      {
        right,
        configuration <~ left_configuration <~ %__MODULE__{bindings: bindings}
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

    code =
      clauses
      |> Enum.map(fn {:"->", _, [[left], right]} -> {Interpreter.match?(left, arg), left, right} end)
      |> filter_clauses()
      |> case do
        [{{:yes, bindings}, _pattern, body}] ->
          bindings =
            Map.new(bindings, fn {{name, _, context} = v, value} when is_variable(v) ->
              {{name, context}, value}
            end)

          configuration = configuration <~ %__MODULE__{bindings: bindings}
          {body, _inner_configuration} = do_run(body, configuration)
          body

        [] ->
          # Nothing will ever match in this case
          # FIXME proper raise
          IO.warn "This case will never match"
          quote(do: raise "Will never match")

        others ->
          clauses =
            for {{_confidence, _bindings}, pattern, body} <- others do
              {left, left_configuration} = propagate_to_pattern(pattern, configuration)
              {right, _right_configuration} = do_run(body, configuration <~ left_configuration)
              {:->, [], [[left], right]}
            end

          {:case, [], [arg, [do: clauses]]}
      end

    {code, configuration}
  end
  defp do_run({{".", _, [{:fn, _, clauses}]}, _, args}, configuration) do
    {args, configuration} = do_run(args, configuration)

    code =
      clauses
      |> Enum.map(fn {:"->", _, [left, right]} -> {Interpreter.multimatch?(left, args), left, right} end)
      |> filter_clauses()
      |> case do
        [{{:yes, bindings}, _pattern, body}] ->
          bindings =
            Map.new(bindings, fn {{name, _, context} = v, value} when is_variable(v) ->
              {{name, context}, value}
            end)

          configuration = configuration <~ %__MODULE__{bindings: bindings}
          {body, _inner_configuration} = do_run(body, configuration)
          body

        [] ->
          # Nothing will ever match in this case
          # FIXME proper raise
          IO.warn "This fn will never success"
          quote(do: raise "Will never match")

        others ->
          clauses =
            for {{_confidence, _bindings}, pattern, body} <- others do
              {left, left_configuration} = propagate_to_pattern(pattern, configuration)
              {right, _right_configuration} = do_run(body, configuration <~ left_configuration)
              {:->, [], [quote(do: {unquote_splicing left}), right]}
            end

          {:case, [], [quote(do: {unquote_splicing args}), [do: clauses]]}
      end

    {code, configuration}
  end

  # For
  defp do_run({:for, _meta, _iters_and_stuff}, _configuration) do
    raise "Not implemented yet"
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
  defp do_run(dot_call(module, func, args), configuration) do
    {args, args_configuration} = do_run(args, configuration)
    call = dot_call(module, func, args)
    if Enum.all?(args, &Macro.quoted_literal?/1) and is_pure(configuration, call) do
      {Interpreter.eval(call), args_configuration}
    else
      {call, args_configuration}
    end
  end
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
    call = {func, meta, args}
    if Enum.all?(args, &Macro.quoted_literal?/1) and is_pure(configuration, call) do
      {Interpreter.eval(call), args_configuration}
    else
      {call, args_configuration}
    end
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
      nil -> {pinned, %__MODULE__{}}
      val -> {val, %__MODULE__{}}
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
      Enum.reduce(items, {[], %__MODULE__{}}, fn item, {items, acc_configuration} ->
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
  defp propagate_to_pattern(other, _configuration), do: {other, %__MODULE__{}}

  ## Helpers for working with configuration

  defp (%{bindings: left_bindings} = left) <~ (%{bindings: right_bindings}) do
    %{left | bindings: Map.merge(left_bindings, right_bindings)}
  end

  defp get_bind(%{bindings: bindings} = cfg, {name, _, context}, default \\ nil) do
    with {bound_name, bound_context} <- Map.get(bindings, {name, context}, default) do
      get_bind(cfg, {bound_name, [], bound_context}, {bound_name, [], bound_context})
    end
  end

  defp put_bind(%{bindings: bindings} = cfg, {name, _, context}, {bind_to_name, _, bind_to_context}) do
    %{cfg | bindings: Map.put(bindings, {name, context}, {bind_to_name, bind_to_context})}
  end

  defp is_pure(configuration, dot_call(module, func, args)) do
    Enum.all?(args, &Macro.quoted_literal?/1) and ({module, func, length(args)} in configuration.pure_functions)
  end
  defp is_pure(_configuration, _ast), do: false

  defp unify({varname, meta, _cotnext}) do
    {varname, meta, gen_uniq_context()}
  end

  # Helpers optimizations

  defp drop_literals_in_block(_configuration, []), do: []
  defp drop_literals_in_block(_configuration, [last]), do: [last]
  defp drop_literals_in_block(configuration, [first, second | tail]) do
    if Macro.quoted_literal?(first) or is_pure(configuration, first) do
      drop_literals_in_block(configuration, [second | tail])
    else
      [first | drop_literals_in_block(configuration, [second | tail])]
    end
  end

  defp filter_clauses([{{:yes, _}, _, _} = last | _tail]), do: [last]
  defp filter_clauses([{:no, _, _} | tail]), do: filter_clauses(tail)
  defp filter_clauses([other | tail]), do: [other | filter_clauses(tail)]
  defp filter_clauses([]), do: []

end
