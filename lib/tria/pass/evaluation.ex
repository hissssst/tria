defmodule Tria.Pass.Evaluation do

  @moduledoc """
  Pass which evaluates, propagates and drops dead code in one run
  This pass performs almost no analysis and just tries to preevaluate any
  forms it comes accross

  #TODO optimize try/rescue
  """

  import Tria.Common
  import Tria.Tri
  alias Tria.Interpreter

  defstruct [
    bindings: %{},
    context: :regular
  ]

  def run_once!(ast, configuration \\ [])
  def run_once!(ast, configuration) when is_list(configuration) do
    run_once!(ast, struct!(__MODULE__, configuration))
  end
  def run_once!(ast, configuration) do
    {code, _bindings} = do_run(ast, configuration)
    code
  rescue
    e ->
      inspect_ast(ast, label: :run_once_fail)
      reraise e, __STACKTRACE__
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
    {left, left_configuration} = propagate_to_pattern(left, configuration)
    {right, right_configuration} = do_run(right, configuration)

    case Interpreter.match?(left, right) do
      {:yes, bindings} ->
        {configuration, binds} =
          Enum.reduce(bindings, {configuration <~ left_configuration, []}, fn
            {variable, value} = bind, {configuration, binds} ->
              cond do
                Macro.quoted_literal?(value) or is_variable(value) ->
                  {put_bind(configuration, variable, value), binds}

                is_fn(value) ->
                  {put_bind(configuration, variable, value), [bind_to_equal(bind) | binds]}

                true ->
                  {configuration, [bind_to_equal(bind) | binds]}
              end
          end)

        body =
          quote do
            unquote_splicing(binds)
            unquote(right)
          end
        {body, configuration}

      :no ->
        IO.warn "This bind will never match"
        body = quote do: raise "Will never match"
        {body, configuration <~ right_configuration <~ left_configuration}

      {:maybe, _} ->
        #TODO relax matching
        {
          quote(do: unquote(left) = unquote(right)),
          configuration <~ right_configuration <~ left_configuration
        }
    end

    if Macro.quoted_literal?(right) do
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

    # inspect_ast(quote(do: case(unquote(arg), do: unquote(clauses))), label: :before)

    code =
      clauses
      |> Enum.map(fn {:"->", _, [[left], right]} ->
        {Interpreter.match?(left, arg), left, right}
      end)
      |> filter_clauses()
      # |> IO.inspect(label: :after_filter)
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
      # |> inspect_ast(label: :after)

    {code, configuration}
  end

  # Fn
  # I can't remember why I left dotted call here
  defp do_run({{:".", _, [{:fn, _, clauses}]}, _, args}, configuration) do
    {args, configuration} = do_run(args, configuration)

    code =
      clauses
      |> Enum.map(fn {:"->", _, [left, right]} -> {Interpreter.multimatch?(left, args), left, right} end)
      |> filter_clauses()
      |> case do
        [{{:yes, bindings}, _pattern, body}] ->
          configuration =
            Enum.reduce(bindings, configuration, fn {key, value}, configuration when is_variable(key) ->
              put_bind(configuration, key, value)
            end)

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
  defp do_run({:fn, _, clauses}, configuration) do
    clauses =
      clauses
      |> Enum.map(fn {:"->", _, [left, right]} ->
        {left, new_configuration} = propagate_to_pattern(left, configuration)
        {right, _right_configuration} = do_run(right, configuration <~ new_configuration)

        {:"->", [], [left, right]}
      end)

    {{:fn, [], clauses}, configuration}
  end

  # For
  defp do_run({:for, _meta, _iters_and_stuff}, _configuration) do
    raise "Not implemented yet"
  end

  # With
  defp do_run({:with, meta, clauses}, configuration) do
    {[{:do, do_clause} | else_clause?], clauses} = List.pop_at(clauses, -1)
    else_clauses = Keyword.get(else_clause?, :else)

    {reversed_clauses, do_configuration} =
      Enum.reduce(clauses, {[], configuration}, fn
        {op, meta, [left, right]}, {clauses, configuration} ->
          {right, _} = do_run(right, configuration)
          {left, left_configuration} = propagate_to_pattern(left, configuration)

          {
            [{op, meta, [left, right]} | clauses],
            configuration <~ left_configuration
          }
      end)

    clauses = Enum.reverse(reversed_clauses)
    {do_clause, _} = do_run(do_clause, do_configuration)

    # #TODO optimize else_clauses
    # {tri(case(_, do: else_clauses)), _} =
    #   do_run(quote(do: case(x, do: unquote(else_clauses))), configuration)
    case else_clauses do
      e when e in [[], nil] ->
        {{:with, meta, clauses ++ [[do: do_clause]]}, configuration}

      else_clauses ->
        {{:with, meta, clauses ++ [[do: do_clause, else: else_clauses]]}, configuration}
    end

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
  defp do_run({:"%{}", _, [{:"|", _, [map, pairs]}]}, configuration) do
    {map, map_configuration} = do_run(map, configuration)
    pairs_with_configuration =
      Enum.map(pairs, fn {key, value} ->
        {do_run(key, configuration), do_run(value, configuration)}
      end)

    pairs = Enum.map(pairs_with_configuration, fn {{key, _}, {value, _}} -> {key, value} end)

    configuration =
      Enum.reduce(pairs_with_configuration, map_configuration, fn {{_, key}, {_, value}}, configuration ->
        configuration <~ key <~ value
      end)

    {
      # quote(do: %{unquote(map) | unquote_splicing(pairs)}),
      {:"%{}", [], [{:"|", [], [map, pairs]}]},
      configuration
    }
  end

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

  # defp do_run({:"|", _, [left, right]}, configuration) do
  #   {left, left_configuration} = do_run(left, configuration)
  #   {right, right_configuration} = do_run(right, configuration)
  #   {{:"|", [], [left, right]}, left_configuration <~ right_configuration}
  # end

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
      ast =
        case Interpreter.eval(call) do
          {:ok, {result, []}} ->
            Macro.escape(result)

          _ ->
            call
        end

      {ast, args_configuration}
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
      {Interpreter.eval!(call), args_configuration}
    else
      {call, args_configuration}
    end
  end

  # Variable
  defp do_run(var, configuration) when is_variable(var) do
    case fetch_bind(configuration, var) do
      :error ->
        {var, configuration}

      {:ok, value} ->
        # Trick to fetch nested binds
        do_run(value, configuration)
    end
  end

  # Literal
  defp do_run(other, configuration), do: {other, configuration}

  # Propagating to pattern

  defp propagate_to_pattern(ast, old_configuration, new_configuration \\ %__MODULE__{})

  defp propagate_to_pattern({:when, _, pattern_and_guard}, configuration, new_configuration) do
    {guard, patterns} = List.pop_at(pattern_and_guard, -1)
    {patterns, pattern_configuration} = propagate_to_pattern(patterns, configuration, new_configuration)

    # Since guard doesn't create new variables, we just ignore it
    # IO.inspect configuration <~ pattern_configuration, label: :what_the_fuck
    {guard, _guard_configuration} = do_run(guard, configuration <~ pattern_configuration)

    {{:when, [], patterns ++ [guard]}, pattern_configuration}
  end

  defp propagate_to_pattern({:^, _, [variable]} = pinned, configuration, new_configuration) when is_variable(variable) do
    with {:ok, _} <- fetch_bind(new_configuration, variable) do
      raise "What the actual fuck"
    end

    case fetch_bind(configuration, variable) do
      :error ->
        {pinned, new_configuration}

      {:ok, val} when is_variable(val) ->
        {{:^, [], [val]}, new_configuration}

      {:ok, val} ->
        if Macro.quoted_literal?(val) do
          {val, new_configuration}
        else
          {pinned, new_configuration}
        end
    end
  end

  defp propagate_to_pattern(variable, _configuration, new_configuration) when is_variable(variable) do
    case fetch_bind(new_configuration, variable) do
      :error ->
        # It appears that it is the new variable
        new_variable = unify(variable)
        {new_variable, put_bind(new_configuration, variable, new_variable)}

      {:ok, variable} ->
        # It appears that the variable is present multiple times in the pattern
        {variable, new_configuration}
    end
  end

  # Call
  defp propagate_to_pattern({n, m, items}, configuration, new_configuration) when is_list(items) do
    {items, new_configuration} = propagate_to_pattern(items, configuration, new_configuration)
    {{n, m, items}, new_configuration}
  end

  # List
  defp propagate_to_pattern(items, configuration, new_configuration) when is_list(items) do
    Enum.map_reduce(items, new_configuration, fn item, acc_configuration ->
      propagate_to_pattern(item, configuration, acc_configuration)
    end)
  end

  # Twople
  defp propagate_to_pattern({left, right}, configuration, new_configuration) do
    {left, new_configuration} = propagate_to_pattern(left, configuration, new_configuration)
    {right, new_configuration} = propagate_to_pattern(right, configuration, new_configuration)

    {
      {left, right},
      new_configuration
    }
  end
  
  # Literal
  defp propagate_to_pattern(other, _configuration, new), do: {other, new}

  ## Helpers for working with configuration

  defp merge_configuration([]), do: %__MODULE__{}
  defp merge_configuration(configurations) when is_list(configurations) do
    Enum.reduce(configurations, fn right, left -> left <~ right end)
  end

  defp (%{bindings: left_bindings} = left) <~ (%{bindings: right_bindings}) do
    %{left | bindings: Map.merge(left_bindings, right_bindings)}
  end

  defp fetch_bind(%{bindings: bindings}, {name, _, context}) do
    Map.fetch(bindings, {name, context})
  end

  defp put_bind(cfg, {name, _, _}, {bind_to_name, _, _}) when :_ in [name, bind_to_name], do: cfg
  defp put_bind(%{bindings: bindings} = cfg, {name, _, context}, value) do
    %{cfg | bindings: Map.put(bindings, {name, context}, value)}
  end

  defp is_pure(_configuration, dot_call(module, _, args) = dc) when is_atom(module) do
    # alias Tria.Analyzer
    alias Tria.Analyzer.Purity

    with true <- Enum.all?(args, &Macro.quoted_literal?/1) do
      # tria = Analyzer.fetch_tria({module, func, args})
      with false <- Purity.check_analyze(dc) do
        case Purity.run_analyze(dc) do
          {:pure, _, _} ->
            true

          _ ->
            false
        end
      end
    end
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

  defp bind_to_equal({left, right}) do
    quote do: unquote(left) = unquote(right)
  end

  defp binds_to_block(binds) do
    {:__block__, [], Enum.map(binds, &bind_to_equal/1)}
  end

end
