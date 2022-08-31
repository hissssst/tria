defmodule Tria.Analyzer.Purity do

  import Tria.Common
  import Tria.Tri

  alias Tria.{Analyzer, FunctionRepo}
  alias Tria.Analyzer.Purity.Provider

  @doc """
  Runs valid code and checks if any of effect functions is called
  """
  def run_analyze(ast) do
    me = {:me, [], nil}
    body =
      Macro.postwalk(ast, fn
        dot_call(m, f, a) = mfa ->
          if lookup {m, f, a} do
            mfa
          else
            quote do: send(unquote(me), {:effect, unquote Macro.escape mfa})
          end

        other ->
          other
      end)

    body =
      quote do
        unquote(me) = self()
        unquote(body)
      end

    {type, result} =
      try do
        {:ok, Code.eval_quoted(body, [])}
      rescue
        error -> {:error, error}
      catch
        :exit, exit -> {:exit, exit}
        thrown -> {:thrown, thrown}
      end

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
            ask(mfa)

          # Is a NIF or BIF function
          {:fn, _, [{:"->", _, [_, dot_call(:erlang, :nif_error, _)]}]} ->
            ask(mfa)

          # Is an Elixir bootstrap
          {:fn, _, [{:"->", _, [_, ^dotted]}]} ->
            ask(mfa)

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

  defp ask(mfa) do
    stack = Process.get(:stack, [])
    Provider.is_pure(mfa, stack: stack)
  end
  
end
