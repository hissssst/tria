defmodule Tria.Interpreter do

  @moduledoc """
  Helpers for partial interpreting of Tria's code
  #TODO: multimatching_clause handle matcheds
  #TODO: match? handle whens
  #TODO: match? handle maps
  #TODO: match? handle structures
  #TODO: match? handle binaries
  #TODO: match? handle fn
  #TODO: matches_conflict? improve
  """

  import Kernel, except: [&&: 2, match?: 2]
  import Tria.Ternary
  import Tria.Common
  import Tria.Tri

  def eval(quoted, bindings \\ [], timeout \\ 5000) do
    # It's neccessary to run this function in a cleanest enviroment possible
    # So no Task or anything like this, plain `spawn`
    ref = make_ref()
    caller = self()
    pid =
      spawn fn ->
        result =
          try do
            {:ok, Code.eval_quoted(quoted, bindings)}
          rescue
            error -> {:error, error}
          catch
            :exit, exit -> {:exit, exit}
            thrown -> {:thrown, thrown}
          end

        send(caller, {ref, result})
      end

    receive do
      {^ref, result} ->
        result

      after timeout ->
        Process.exit(pid, :brutal_kill)
        raise "Timeout on evaluation"
    end
  end

  def eval!(quoted, bindings \\ [], timeout \\ 5000) do
    {:ok, {result, _bindings}} = eval(quoted, bindings, timeout)
    result
  end

  @spec matching_clause(Macro.t(), [Macro.t()], :must | :can) :: {:ok, Macro.t()} | :error
  def matching_clause(data, clauses, confidence \\ :must) do
    clauses
    |> Enum.map(fn clause ->
      answer =
        clause
        |> match?(data)
        |> result_to_group()

      {answer, clause}
    end)
    |> choose_by_confidence(confidence)
  end

  def multimatching_clause(args, [first | _] = clauses, confidence \\ :must) do
    first_length = length(first)

    if Enum.any?(clauses, &(length(&1) != first_length)) do
      raise ArgumentError, "Clauses have different arities!"
    end

    if length(args) != first_length do
      raise ArgumentError, "Data and clauses have different length"
    end

    clauses
    |> Enum.map(fn clause ->
      answer =
        clause
        |> Enum.zip(args)
        |> Enum.reduce_while({:yes, []}, fn {pattern, arg}, acc ->
          case match?(pattern, arg) do
            :no -> {:halt, :no}
            other -> {:cont, merge(acc, other)}
          end
        end)
        |> result_to_group()

      {answer, clause}
    end)
    |> choose_by_confidence(confidence)
  end

  defp choose_by_confidence(results, :must) do
    results
    |> Enum.drop_while(&Kernel.match?({:no, _}, &1))
    |> case do
      [{:yes, first_match} | _] -> {:ok, first_match}
      _ -> :error
    end
  end

  defp choose_by_confidence(results, :can) do
    results
    |> Enum.drop_while(&Kernel.match?({:no, _}, &1))
    |> case do
      [{_, first_match} | _] -> {:ok, first_match}
      _ -> :error
    end
  end

  def match(data, clause) do
    quote do
      unquote(clause) = unquote(data)
    end
    |> eval()
    |> elem(1)
  end

  # Multimatch?

  def multimatch?(left, right) when is_list(left) and is_list(right) do
    left
    |> Enum.zip(right)
    |> Enum.reduce_while({:yes, []}, fn {left, right}, {level, matches} ->
      with(
        {new_level, new_matches} <- match?(left, right),
        matches = matches ++ new_matches,
        level = new_level && level,
        new_level when new_level in ~w[yes maybe]a <- matches_do_not_conflict?(matches)
      ) do
        {:cont, {level && new_level, matches}}
      else
        _ -> {:halt, :no}
      end
    end)
  end

  # Match?

  # When
  def match?({:when, _, [pattern, conditions]}, ast) do
    case match?(pattern, ast) do
      {:yes, binds} ->
        # IO.inspect binds, label: :binds
        if Enum.all?(binds, fn {_, v} -> Macro.quoted_literal?(v) end) do
          # Pre-evalute binds, because `match?` returns them in quoted form
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
    # with {level, matches} <- match?(pattern) do
    #   {level, matches} && match_when?(matches, conditions)
    # end
  end

  # List patterns
  def match?(tri([head_pattern | tail_pattern]), tri([head_ast | tail_ast])) do
    merge(match?(head_pattern, head_ast), match?(tail_pattern, tail_ast))
  end

  def match?(tri([head_pattern | tail_pattern]), [head_ast | tail_ast]) do
    IO.puts "here"
    merge(match?(head_pattern, head_ast), match?(tail_pattern, tail_ast))
  end

  def match?([head_pattern | tail_pattern], tri([head_ast | tail_ast])) do
    IO.puts "here 2"
    merge(match?(head_pattern, head_ast), match?(tail_pattern, tail_ast))
  end

  def match?([head_pattern | tail_pattern], [head_ast | tail_ast]) do
    merge(match?(head_pattern, head_ast), match?(tail_pattern, tail_ast))
  end

  def match?([], []), do: {:yes, []}
  def match?([_ | _], []), do: :no
  def match?([], [_ | _]), do: :no

  # Other collections
  def match?({left_pattern, right_pattern}, {left_ast, right_ast}) do
    merge(match?(left_pattern, left_ast), match?(right_pattern, right_ast))
  end

  def match?({_, _}, {:{}, _, _}), do: :no
  def match?({:{}, _, _}, {_, _}), do: :no

  def match?({:{}, _, patterns}, {:{}, _, asts}) do
    if length(patterns) != length(asts) do
      :no
    else
      patterns
      |> Enum.zip(asts)
      |> Enum.reduce({:yes, []}, fn {pattern, ast}, acc ->
        merge(acc, match?(pattern, ast))
      end)
    end
  end

  def match?({:<<>>, _, _patterns}, {:<<>>, _, _asts}) do
    raise "Binary matching is not implemented"
  end
  
  def match?({:%{}, _, _patterns}, {:%{}, _, [{:"|", _, _kvs}]}) do
    raise "Maps matching not implemented"
  end

  def match?({:%{}, _, patterns}, {:%{}, _, asts}) do
    if length(patterns) > length(asts), do: throw :no

    literal_keys = fn list ->
      Enum.split_with(list, fn {k, _} -> Macro.quoted_literal?(k) end)
    end

    {literal_pattern, patterns} = literal_keys.(patterns)
    {literal_ast, asts} = literal_keys.(asts)

    literal_ast = Map.new(literal_ast)

    {level_binds, hopes} =
      Enum.reduce_while(literal_pattern, {{:yes, []}, patterns}, fn {key, pattern}, {acc, hopes} ->
        case literal_ast do
          %{^key => value} ->
            {merge(acc, match?(pattern, value)), hopes}

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

  # Equals
  def match?(tri(left = right), val) do
    merge(match?(left, val), match?(right, val))
  end

  # Variables
  def match?(tri(^_) = pinned, val), do: {:maybe, [{pinned, val}]}
  def match?(var, val) when is_variable(var), do: {:yes, [{var, val}]}
  def match?(pattern, var) when is_variable(var), do: {:maybe, [{pattern, var}]}

  # Non-variable collection on right side
  # Because all successfull cases for collections were handled above
  def match?(_, coll) when is_collection(coll), do: :no
  def match?(pattern, {_, _, l} = val) when is_list(l), do: {:maybe, [{pattern, val}]}

  # Literals
  def match?(same, same), do: {:yes, []}
  def match?(_, _), do: :no

  # Helpers

  defp merge({:yes, m1}, {:yes, m2}) do
    {:yes, merge_matches(m1, m2)}
  end

  defp merge({_, m1}, {_, m2}) do
    {:maybe, merge_matches(m1, m2)}
  end

  defp merge(_, _), do: :no

  defp merge_matches(left_matches, right_matches) do
    left_matches ++ right_matches
  end

  defp result_to_group(:no), do: :no

  defp result_to_group({level, matches}) do
    matches_do_not_conflict?(matches) && level
  end

  defp matches_do_not_conflict?(matches) do
    for {{name, _, ctx} = var, value} when is_variable(var) and name != :_ <- matches do
      {{name, ctx}, value}
    end
    |> Enum.group_by(fn {k, _} -> k end, fn {_, v} -> v end)
    |> Enum.reduce(:yes, fn
      {_, [_data]}, ans ->
        ans && :yes

      {{:_, _, _}, _}, ans ->
        ans && :yes

      {_, [_head | _tail]}, ans ->
        # FIXME
        ans && :no
    end)
  end
end
