defmodule Tria.Interpreter do

  @moduledoc """
  Helpers for partial interpreting of Tria's code
  #TODO: multimatching_clause handle matcheds
  #TODO: match handle whens
  #TODO: match handle binaries
  #TODO: match handle fn
  #TODO: matches_conflict? improve
  """

  import Kernel, except: [&&: 2]
  import Macro, only: [quoted_literal?: 1]
  import Tria.Ternary
  import Tria.Common
  import Tria.Tri

  alias Tria.Matchlist
  alias Tria.Translator.Elixir, as: ElixirTranslator
  import Matchlist, only: [is_map_match: 1, is_empty: 1]

  @type eval_result :: {:ok, {any(), Matchlist.t()}}
  | {:error, Exception.t()}
  | {:exit, any()}
  | {:thrown, any()}

  @doc """
  Evaluates quoted code in a separate isolated process and returns result or raises
  """
  @spec eval!(Tria.t(), Matchlist.t(), timeout()) :: any()
  def eval!(quoted, matchlist \\ Matchlist.empty(), timeout \\ 5000) do
    {:ok, {result, _matchlist}} = eval(quoted, matchlist, timeout)
    result
  end

  @doc """
  Evaluates quoted code in a separate isolated process and returns whatever the code returns
  """
  @spec eval(Tria.t(), Matchlist.t(), timeout()) :: eval_result()
  def eval(quoted, matchlist \\ Matchlist.empty(), timeout \\ 5000) do
    # It's neccessary to run this function in a cleanest enviroment possible
    # So no Task or anything like this, plain `spawn`
    ref = make_ref()
    caller = self()
    pid =
      spawn fn ->
        result = eval_local(quoted, matchlist)
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

  @doc """
  Evaluates quoted code in the caller's process and returns whatever the code returns
  """
  @spec eval_local(Tria.t(), Matchlist.t()) :: eval_result()
  def eval_local(quoted, matchlist \\ Matchlist.empty()) do
    # We manually attach matchlist before the code
    # This is just more efficient and easy to understand
    quoted =
      quote do
        unquote Matchlist.to_quoted matchlist
        unquote quoted
      end
      |> ElixirTranslator.from_tria()

    try do
      {result, binds} = Code.eval_quoted(quoted, [])
      {:ok, {result, Enum.map(binds, fn {{name, ctx}, value} -> {{name, [], ctx}, Macro.escape(value)} end)}}
    rescue
      error -> {:error, error}
    catch
      :exit, exit -> {:exit, exit}
      thrown -> {:thrown, thrown}
    end
  end

  # Multimatch

  @doc """
  Same as `match` but for multiple arguments, like in `fn` calls
  """
  @spec multimatch([Tria.t()], [Tria.t()]) :: {:yes | :maybe, Matchlist.t()} | :no
  def multimatch([left], [right]), do: match(left, right)
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
  @spec match(Tria.t(), Tria.t()) :: {:yes | :maybe, Matchlist.t()} | :no

  # When
  def match({:when, _, [pattern, conditions]}, ast) do
    case match(pattern, ast) do
      {:yes, binds} ->
        # IO.inspect binds, label: :binds
        if Enum.all?(binds, fn {_, v} -> quoted_literal?(v) end) do
          # Pre-evalute binds, because `match` returns them in quoted form
          eval_binds =
            Enum.map(binds, fn {key, value} ->
              # Here `eval!` is safe, because it is a quoted literal and we
              # just want to instantiate it
              {key, eval!(value)}
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

      {:maybe, matches} ->
        {:maybe, matches}

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

  def match([], []), do: {:yes, Matchlist.empty()}

  # Twople
  def match({left_pattern, right_pattern}, {left_ast, right_ast}) do
    merge(match(left_pattern, left_ast), match(right_pattern, right_ast))
  end
  def match({left_pattern, right_pattern}, {:"{}", _, [left_ast, right_ast]}) do
    merge(match(left_pattern, left_ast), match(right_pattern, right_ast))
  end
  def match({:"{}", _, [left_pattern, right_pattern]}, {left_ast, right_ast}) do
    merge(match(left_pattern, left_ast), match(right_pattern, right_ast))
  end

  # Tuple
  def match({:{}, _, patterns}, {:{}, _, asts}) do
    if length(patterns) != length(asts) do
      :no
    else
      patterns
      |> Enum.zip(asts)
      |> Enum.reduce({:yes, Matchlist.empty()}, fn {pattern, ast}, acc ->
        merge(acc, match(pattern, ast))
      end)
    end
  end

  # Binary
  def match({:<<>>, _, _patterns} = left, {:<<>>, _, _asts} = right) do
    IO.warn "Binary matching is not implemented"
    {:maybe, Matchlist.new([ {left, right} ])}
  end

  # Map
  def match({:%{}, _, _patterns} = left, {:%{}, _, [{:"|", _, _kvs}]} = right) do
    #TODO do some checks please
    {:maybe, Matchlist.new([ {left, right} ])}
  end

  # Single key map is just matching like a tuple
  def match({:%{}, _, [left]}, {:%{}, _, [right]}) do
    match(left, right)
  end

  # Matching maps with multiple keys is just a permutation of tuple matches
  # But it is just a ton of nasty logic, we need to think of a way to overcome this
  def match({:%{}, _, patterns}, {:%{}, _, asts}) do
    # When there are more patterns than non patterns, it is hard to get
    if length(patterns) > length(asts), do: throw :no

    literal_keys = fn list ->
      Enum.split_with(list, fn {k, _} -> quoted_literal?(k) end)
    end

    {literal_pattern, patterns} = literal_keys.(patterns)
    {literal_ast, asts} = literal_keys.(asts)

    literal_ast = Map.new(literal_ast)

    {level_binds, hopes} =
      Enum.reduce_while(literal_pattern, {{:yes, Matchlist.empty()}, patterns}, fn {key, pattern}, {acc, hopes} ->
        case literal_ast do
          %{^key => value} ->
            {merge(acc, match(pattern, value)), hopes}

          _ ->
            # In case we don't have a match we can just hope
            # that it is present in non-literal-keys part of AST
            {merge(acc, {:maybe, Matchlist.empty()}), [{key, pattern} | hopes]}
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
        merge({:maybe, [{ {:"%{}", [], hopes}, {:"%{}", [], asts} }]}, level_binds)
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
  def match(tri(^_) = pinned, value), do:                   {:maybe, Matchlist.new [ {pinned, value} ]}
  def match({:_, _, _} = var, _) when is_variable(var), do: {:yes,   Matchlist.empty() }
  def match(var, value) when is_variable(var), do:          {:yes,   Matchlist.new [ {var, value} ]}
  def match(pattern, var) when is_variable(var), do:        {:maybe, Matchlist.new [ {pattern, var} ]}

  def match(pattern, {_, _, l} = val) when is_list(l), do:  {:maybe, Matchlist.new [ {pattern, val} ]}

  # Literals
  def match(same, same), do: {:yes, Matchlist.empty()}
  def match(_, _), do: :no

  # Helpers

  # Merges two match results together
  defp merge({left_level, left_matches}, {right_level, right_matches}) when is_empty(left_matches) or is_empty(right_matches) do
    {left_level && right_level, Matchlist.merge(left_matches, right_matches)}
  end
  defp merge({left_level, left_matches}, {right_level, right_matches}) do
    join_matchlists(left_level && right_level, left_matches, right_matches)
  end

  defp merge(_, _), do: :no

  # Let's read this one
  defp join_matchlists(level, left_matches, right_matches) do
    matches = Matchlist.merge(left_matches, right_matches)
    # It is a list of all key to value bindings
    {_map_matches, regular_matches} = Enum.split_with(matches, &is_map_match/1)

    regular_matches =
      regular_matches
      |> Enum.flat_map(fn
        {{:^, _, [variable]}, value} = item when is_variable(variable) ->
          [item, {value, variable}, {variable, value}]

        {key, value} ->
          [{key, value}, {value, key}]
      end)
      |> group_matches()

    level =
      Enum.reduce(regular_matches, level, fn
        # One value for pin or variable
        {_key, [_]}, level ->
          level

        # Multiple values for pin or variable or pattern
        {_key, values}, level ->
          # In case some patterns are matched agains different values
          # We'll just check if it is a values match
          #FIXME fix function calls in ast
          values
          |> pairs()
          |> Enum.map(fn {l, r} -> mutual_translate(l, r) end)
          |> Enum.reduce(level, fn {left, right}, acc ->
            case match(left, right) do
              {level, matches} ->
                matches
                |> Enum.flat_map(fn {l, r} -> [{l, r}, {r, l}] end)
                |> group_matches()
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

    {level, matches}
    catch :no -> :no
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
    |> Enum.group_by(fn {k, _} -> k end, fn {_, v} -> v end)
    |> Enum.map(fn {key, values} -> {key, Enum.uniq values} end)
  end

  # Here I use counters, but all of this will be dropped later
  defp mutual_translate(left, right) do
    {left, translations} =
      prewalk(left, %{}, fn
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
      prewalk(right, fn
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
