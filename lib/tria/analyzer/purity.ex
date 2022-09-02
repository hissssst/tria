defmodule Tria.Analyzer.Purity do

  import Tria.Common
  import Tria.Tri

  alias Tria.Interpreter
  alias Tria.{Analyzer, FunctionRepo}
  alias Tria.Analyzer.Purity.Provider

  @doc """
  Runs valid code and checks if any of effect functions is called.
  It replaces all possible impure calls with sends and gathers
  all effects called
  """
  def run_analyze(ast) do
    {type, result} = 
      ast
      |> Macro.postwalk(fn
        dot_call(m, f, a) = mfa ->
          if lookup {m, f, a} do
            mfa
          else
            quote do: send(self(), {:effect, unquote Macro.escape mfa})
          end

        other ->
          other
      end)
      |> Interpreter.eval_local()

    effects = fetch_effects()

    case type do
      :ok when effects == [] ->
        {:pure, result, effects}

      :ok ->
        {:effect, result, effects}

      other ->
        {other, result, effects}
    end
  end

  @doc """
  Checks if effect calls are present in ast
  """
  def check_analyze(ast, stack \\ []) do
    Macro.prewalk(ast, [], fn
      dot_call(m, f, a), acc ->
        mfa = {unalias(m), f, a}
        if lookup(mfa, stack) do
          {mfa, acc}
        else
          {mfa, [mfa | acc]}
        end

      tri(_.(tri_splicing _)) = func, acc ->
        {func, [func | acc]}

      other, acc ->
        {other, acc}
    end)
    |> case do
      {_, []} -> true
      _ -> false
    end
  end

  defp lookup({m, f, a} = mfa, stack) when is_atom(m) do
    mfarity = arityfy mfa
    if mfarity in stack do
      # Recursive functions are considered pure
      true
    else
      with nil <- FunctionRepo.lookup mfa, :pure do
        dotted = {{:".", [], [m, f]}, [], a}
        case Analyzer.fetch_tria(mfa) do
          # No function found
          nil ->
            Provider.is_pure(mfa, stack: stack)

          # Is a NIF or BIF function
          {:fn, _, [{:"->", _, [_, dot_call(:erlang, :nif_error, _)]}]} ->
            Provider.is_pure(mfa, stack: stack)

          # Is an Elixir bootstrap
          {:fn, _, [{:"->", _, [_, ^dotted]}]} ->
            Provider.is_pure(mfa, stack: stack)

          ast ->
            # IO.inspect mfa
            res = check_analyze(ast, [mfarity | stack])
            FunctionRepo.insert(mfa, :pure, res)
            res
        end
      end
    end
  end
  defp lookup({_, _f, _a}), do: false

  defp fetch_effects() do
    receive do
      {:effect, effect} -> [effect | fetch_effects()]
      after 0 -> []
    end
  end
  
end
