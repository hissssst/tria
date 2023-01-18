defmodule Tria.Language.Analyzer.Safety do

  @moduledoc """
  Analyzer which cheks if the given AST can or can not raise or
  throw and exception
  """

  import Tria.Language
  alias Tria.Language.MFArity
  alias Tria.Language.Codebase
  alias Tria.Language.FunctionRepo

  @doc """
  Returns `true` is the function definitely can not raise
  If the analyzer is unsure that the function can raise or not,
  returns `false`
  """
  @spec analyze(Tria.t(), [MFArity.mfarity()] | []) :: boolean()
  def analyze(ast, stack \\ []) do
    postwalk(ast, fn
      dot_call(module, function, args) = dotcall when is_atom(module) and is_atom(function) ->
        {module, function, length(args)}
        |> lookup(stack)
        |> if(do: dotcall, else: throw false)

      dot_call(_, _, _) ->
        throw false

      {:fn, _, _} ->
        nil

      {:raise, _, _} ->
        throw false

      {:throw, _, _} ->
        throw false

      {:receive, _, [{:do, clauses} | _]} = ast ->
        if complete_clauses?(clauses), do: ast, else: throw false

      {:case, _, [_arg, [do: clauses]]} = ast ->
        if complete_clauses?(clauses), do: ast, else: throw false

      other ->
        other
    end)

    true
    catch false -> false
  end

  defp complete_clauses?(clauses) do
    Enum.any?(clauses, &match?({:"->", _, [[x], _]} when is_variable(x), &1))
  end

  @kernel_safe [
    {:>,   2},
    {:>=,  2},
    {:<,   2},
    {:<=,  2},
    {:===, 2},
    {:==,  2},
    {:!==, 2},
    {:!=,  2},
    {:||,  2},
    {:&&,  2},
    {:max, 2},
    {:min, 2},
  ]

  defp lookup({Kernel, function, arity}, _stack) when {function, arity} in @kernel_safe do
    true
  end

  defp lookup({:erlang, function, arity}, _stack) do
    :erl_bifs.is_safe(:erlang, function, arity)
  end

  defp lookup({module, function, arity} = mfarity, stack) do
    with(
      nil <- FunctionRepo.lookup(mfarity, :safe_cache),
      false <- mfarity in stack
    ) do
      result =
        case Codebase.fetch_tria_bodies(mfarity) do
          nil ->
            false

          # Single clause function which calls itself is recursively defined
          # So we better ask
          [dot_call(^module, ^function, args)] when length(args) == arity ->
            MFArity.inspect(mfarity, label: :assuming_unsafe)
            false

          # Is a NIF or BIF function
          [dot_call(:erlang, :nif_error, _)] ->
            false

          bodies ->
            analyze(bodies, [mfarity | stack])
        end

      FunctionRepo.insert(mfarity, :safe_cache, result)
    end
  end

end
