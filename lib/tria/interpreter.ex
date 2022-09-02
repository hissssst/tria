defmodule Tria.Interpreter do

  @moduledoc """
  Helpers for partial interpreting of Tria's code
  #TODO: multimatching_clause handle matcheds
  #TODO: match handle whens
  #TODO: match handle maps
  #TODO: match handle structures
  #TODO: match handle binaries
  #TODO: match handle fn
  #TODO: matches_conflict? improve
  """

  import Kernel, except: [&&: 2]
  import Macro, only: [quoted_literal?: 1]
  import Tria.Ternary
  import Tria.Common
  import Tria.Tri

  @type variable :: {atom(), list(), atom()}
  @type pin :: {:"^", list(), [variable()]}

  @typedoc "Is a type of a default left = right binding"
  @type binding(left, right) :: {left, right}

  @typedoc "Binding where value is set to the variable"
  @type variable_binding :: binding(variable(), Macro.t())

  @typedoc "Binding which binds some value to the pin"
  @type pin_binding :: binding(pin(), Macro.t())
  
  @typedoc "Pattern binding"
  @type pattern_binding :: binding(Macro.t(), Macro.t())

  @typedoc "Binding which binds values of the map together"
  @type map_binding :: {:map_match, [pattern_binding()], [pattern_binding()]}

  @typedoc "Type of binding"
  @type binding() :: variable_binding() | pin_binding() | map_binding()

  def eval(quoted, bindings \\ [], timeout \\ 5000) do
    # It's neccessary to run this function in a cleanest enviroment possible
    # So no Task or anything like this, plain `spawn`
    ref = make_ref()
    caller = self()
    pid =
      spawn fn ->
        result = eval_local(quoted, bindings)
        send(caller, {ref, result})
      end

    receive do
      {^ref, result} ->
        result

      after timeout ->
        Process.exit(pid, :kill)
        raise "Timeout on evaluation"
    end
  end

  def eval_local(quoted, bindings \\ []) do
    try do
      {:ok, Code.eval_quoted(quoted, bindings)}
    rescue
      error -> {:error, error}
    catch
      :exit, exit -> {:exit, exit}
      thrown -> {:thrown, thrown}
    end
  end

  def eval!(quoted, bindings \\ [], timeout \\ 5000) do
    {:ok, {result, _bindings}} = eval(quoted, bindings, timeout)
    result
  end

  # Multimatch

  @doc """
  Same as `match` but for multiple arguments, like in `fn` calls
  """
  def multimatch([{:when, _, [lefts]}], right) do
    {guard, left} = List.pop_at(lefts, -1)
    left = quote do: {unquote_splicing(left)} when unquote(guard)
    right = quote do: {unquote_splicing(right)}
    match(left, right)
  end
  def multimatch(left, right) when is_list(left) and is_list(right) do
    left = quote do: {unquote_splicing(left)}
    right = quote do: {unquote_splicing(right)}
    match(left, right)
  end

  # Match?

  @doc """
  Tries to match ast on pattern and returns the ternary result with bindings
  """
  @spec match(Tria.t(), Tria.t()) :: {:yes | :maybe, [binding()]} | :no

  # When
  def match({:when, _, [pattern, conditions]}, ast) do
    case match(pattern, ast) do
      {:yes, binds} ->
        # IO.inspect binds, label: :binds
        if Enum.all?(binds, fn {_, v} -> quoted_literal?(v) end) do
          # Pre-evalute binds, because `match` returns them in quoted form
          eval_binds =
            Enum.map(binds, fn {{name, _, ctx} = v, value} when is_variable(v) ->
              {{name, ctx}, eval!(value)}
            end)

          case eval(conditions, eval_binds) do
            {:ok, {true, _}} ->
              {:yes, binds}

            {:ok, {false, _}} ->
              :no

            _ ->
              {:maybe, binds}
          end
        else
          {:maybe, binds}
        end

      {:maybe, binds} ->
        {:maybe, binds}

      _ ->
        #FIXME
        :no
    end
    # raise "Not implemented"
    # with {level, matches} <- match(pattern) do
    #   {level, matches} && match_when?(matches, conditions)
    # end
  end

  # Collections

  # List
  def match(tri([head_pattern | tail_pattern]), tri([head_ast | tail_ast])) do
    merge(match(head_pattern, head_ast), match(tail_pattern, tail_ast))
  end

  def match(tri([head_pattern | tail_pattern]), [head_ast | tail_ast]) do
    merge(match(head_pattern, head_ast), match(tail_pattern, tail_ast))
  end

  def match([head_pattern | tail_pattern], tri([head_ast | tail_ast])) do
    merge(match(head_pattern, head_ast), match(tail_pattern, tail_ast))
  end

  def match([head_pattern | tail_pattern], [head_ast | tail_ast]) do
    merge(match(head_pattern, head_ast), match(tail_pattern, tail_ast))
  end

  def match([], []), do: {:yes, []}

  # Twople
  def match({left_pattern, right_pattern}, {left_ast, right_ast}) do
    merge(match(left_pattern, left_ast), match(right_pattern, right_ast))
  end

  # Tuple
  def match({:{}, _, patterns}, {:{}, _, asts}) do
    if length(patterns) != length(asts) do
      :no
    else
      patterns
      |> Enum.zip(asts)
      |> Enum.reduce({:yes, []}, fn {pattern, ast}, acc ->
        merge(acc, match(pattern, ast))
      end)
    end
  end

  # Binary
  def match({:<<>>, _, _patterns}, {:<<>>, _, _asts}) do
    raise "Binary matching is not implemented"
  end

  # Map
  def match({:%{}, _, _patterns}, {:%{}, _, [{:"|", _, _kvs}]}) do
    raise "Maps matching not implemented"
  end

  def match({:%{}, _, patterns}, {:%{}, _, asts}) do
    if length(patterns) > length(asts), do: throw :no

    literal_keys = fn list ->
      Enum.split_with(list, fn {k, _} -> quoted_literal?(k) end)
    end

    {literal_pattern, patterns} = literal_keys.(patterns)
    {literal_ast, asts} = literal_keys.(asts)

    literal_ast = Map.new(literal_ast)

    {level_binds, hopes} =
      Enum.reduce_while(literal_pattern, {{:yes, []}, patterns}, fn {key, pattern}, {acc, hopes} ->
        case literal_ast do
          %{^key => value} ->
            {merge(acc, match(pattern, value)), hopes}

          _ ->
            # In case we don't have a match we can just hope
            # that it is present in non-literal-keys part of AST
            {merge(acc, {:maybe, []}), [{key, pattern} | hopes]}
        end
        |> case do
          {:no, _} = result -> {:halt, result}
          other -> {:cont, other}
        end
      end)

    cond do
      length(hopes) == 0 ->
        level_binds

      length(hopes) > length(asts) ->
        :no

      true ->
        merge({:maybe, [{:map_match, hopes, asts}]}, level_binds)
    end
  catch
    :no -> :no
  end

  # In case collections do not match
  def match(pattern, value) when is_collection(pattern) and is_collection(value), do: :no

  # Equals
  def match(tri(left = right), value) do
    merge(match(left, value), match(right, value))
  end

  # Variables
  def match(tri(^_) = pinned, value), do: {:maybe, [{pinned, value}]}
  def match({:_, _, _} = var, _) when is_variable(var), do: {:yes, []}
  def match(var, value) when is_variable(var), do: {:yes, [{var, value}]}
  def match(pattern, var) when is_variable(var), do: {:maybe, [{pattern, var}]}

  def match(pattern, {_, _, l} = val) when is_list(l), do: {:maybe, [{pattern, val}]}

  # Literals
  def match(same, same), do: {:yes, []}
  def match(_, _), do: :no

  # Helpers

  # Merges two match results together
  defp merge({left_level, m1}, {right_level, m2}) do
    merge_matches(left_level && right_level, m1, m2)
  end

  defp merge(_, _), do: :no

  #TODO This shit is not merging map matches (in fixme below)
  defp merge_matches(level, left_matches, right_matches) do
    # It is a list of all key to value bindings
    all_matches =
      left_matches
      |> Kernel.++(right_matches)
      |> Enum.filter(& match?({_, _}, &1))
      |> Enum.group_by(fn {k, _} -> k end, fn {_, v} -> v end)
      |> Enum.map(fn {key, values} -> {key, Enum.uniq values} end)

    map_matches =
      left_matches
      |> Kernel.++(right_matches)
      |> Enum.filter(& match?({:map_match, _, _}, &1))

    level =
      Enum.reduce(all_matches, level, fn
        # One value for pin or variable
        {_key, [_]}, level ->
          level

        # Multiple values for pin or variable or pattern
        {_key, values}, level ->
          # In case some patterns are matched agains different values
          # We'll just check is values match
          #FIXME fix function calls in ast
          values
          |> pairs()
          |> Enum.map(&mutual_translate/1)
          |> Enum.reduce(level, fn {left, right}, acc ->
            case match(left, right) do
              {level, matches} ->
                matches
                |> Enum.flat_map(fn {l, r} -> [{l, r}, {r, l}] end)
                |> group_matches()
                |> IO.inspect(label: :well)
                |> Enum.all?(fn {_, values} -> length(values) == 1 end)
                |> case do
                  true ->
                    acc && level

                  false ->
                    throw :no
                end

              :no ->
                throw :no
            end
          end)
      end)

    matches = for {k, vs} <- all_matches, v <- vs, do: {k, v}
    {level, matches}
  catch
    :no -> :no
  end

  defp pairs([]), do: []
  defp pairs([_]), do: []
  defp pairs([head | tail]) do
    Enum.reduce(tail, pairs(tail), fn item, pairs ->
      [{head, item} | pairs]
    end)
  end

  defp group_matches(matches) do
    matches
    |> Enum.filter(& match?({_, _}, &1)) #FIXME read todo above
    |> Enum.group_by(fn {k, _} -> k end, fn {_, v} -> v end)
    |> Stream.map(fn {key, values} -> {key, Enum.uniq values} end) # Streaming to lazily iterate over it
  end

  # Here I use counters, but all of this will be dropped later
  defp mutual_translate({left, right}) do
    {left, translations} =
      Macro.prewalk(left, %{}, fn
        {_, _, args} = ast, translations when is_list(args) ->
          if is_special_form(ast) do
            {ast, translations}
          else
            var = {:var, [counter: :erlang.unique_integer()], nil}
            {var, Map.put(translations, ast, var)}
          end

        other, translations ->
          {other, translations}
      end)

    right =
      Macro.prewalk(right, fn
        {_, _, args} = ast when is_list(args) ->
          if is_special_form(ast) do
            ast
          else
            Map.get_lazy(translations, ast, fn ->
              {:var, [counter: :erlang.unique_integer()], nil}
            end)
          end

        other ->
          other
      end)

    {left, right}
  end
end
