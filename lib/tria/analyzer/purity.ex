defmodule Tria.Codebase.Purity do

  @moduledoc """
  Module which analyzes AST for Purity
  """

  import Tria.Common
  import Tria.Tri

  alias Tria.Interpreter
  alias Tria.{Codebase, FunctionRepo}
  alias Tria.Codebase.Purity.Provider

  @doc """
  Runs valid code and checks if any of effect functions is called.
  It replaces all possible impure calls with sends and gathers
  all effects called
  """
  def run_analyze(ast, stack \\ []) do
    {type, result} =
      ast
      |> postwalk(fn
        dot_call(m, f, a) = mfa ->
          if lookup({m, f, a}, stack) do
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
  def check_analyze(ast, stack \\ [])
  def check_analyze(dot_call(_, :__impl__, [:target]), _), do: true
  def check_analyze(ast, stack) do
    prewalk(ast, [], fn
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

  defp lookup({m, f, a} = mfa, stack) when is_atom(m) and is_list(a) do
    mfarity = arityfy mfa
    with nil <- do_lookup(mfa, stack) do
      dotted = {{:".", [], [m, f]}, [], a}
      case Codebase.fetch_tria(mfa) do
        # No function found
        nil ->
          pure? = Provider.is_pure(mfa, stack: stack)
          FunctionRepo.insert(mfa, :pure, pure?)

        # Is a NIF or BIF function
        {:fn, _, [{:"->", _, [_, dot_call(:erlang, :nif_error, _)]}]} ->
          pure? = Provider.is_pure(mfa, stack: stack)
          FunctionRepo.insert(mfa, :pure, pure?)

        # Is an Elixir bootstrap
        {:fn, _, [{:"->", _, [_, ^dotted]}]} ->
          pure? = Provider.is_pure(mfa, stack: stack)
          FunctionRepo.insert(mfa, :pure, pure?)

        # Anything else, and for this we don't insert the purity check
        # TODO maybe move this in cache
        ast ->
          res = check_analyze(ast, [mfarity | stack])
          FunctionRepo.insert(mfa, :pure_cache, res)
      end
    end
  end
  defp lookup({_m, _f, _a}, _stack), do: false

  defp do_lookup(mfa, stack) do
    mfarity = arityfy mfa
    with(
      # Recursive functions are considered pure
      false <- mfarity in stack,
      nil <- FunctionRepo.lookup(mfarity, :pure_cache),
      nil <- FunctionRepo.lookup(mfarity, :pure)
    ) do
      nil
    end
  end

  defp fetch_effects() do
    receive do
      {:effect, effect} -> [effect | fetch_effects()]
      after 0 -> []
    end
  end

end
