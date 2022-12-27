defmodule Tria.Language.Analyzer.Purity do

  @moduledoc """
  Module which analyzes AST for Purity

  # TODO

  * ternary logic
  * handle closures (like in Enum.map)
  """

  import Tria.Language
  import Tria.Language.Meta
  import Tria.Language.MFArity, only: :macros

  alias Tria.Language.MFArity
  alias Tria.Language.Interpreter
  alias Tria.Language.{Codebase, FunctionRepo}
  alias Tria.Language.Analyzer.Purity.Provider

  @type stack :: [{module(), atom(), non_neg_integer()}]

  @doc """
  Runs valid code and checks if any of effect functions is called.
  It replaces all possible impure calls with sends and gathers
  all effects called
  """
  def run_analyze(ast, opts \\ []) do
    stack = Keyword.get(opts, :stack, [])
    {type, result} =
      ast
      |> postwalk(fn
        dot_call(m, f, a) = mfa ->
          if lookup({m, f, a}, stack) do
            mfa
          else
            mfa = Macro.escape(mfa)
            quote do
              :erlang.element(2, send(self(), {:effect, unquote(mfa)}))
            end
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
    prewalk(ast, fn
      dot_call(m, f, a) ->
        mfa = {m, f, a}
        if lookup(mfa, stack), do: mfa, else: throw false

      # Even if function creates fn with impure body, this function is still pure
      {:fn, _, _} ->
        nil

      # Receive is always impure, even just sleeping
      {:receive, _, _} ->
        throw false

      # Called to variables are considered impure
      dot_call(_, _) ->
        throw false

      other ->
        other
    end)

    true
    catch false -> false
  end

  @spec check_analyze_mfarity(mfa()) :: boolean()
  def check_analyze_mfarity({module, function, arity}) do
    call = dot_call(module, function, List.duplicate(nil, arity))
    check_analyze(call)
  end

  @spec invalidate(module()) :: :ok
  def invalidate(module) do
    FunctionRepo.remove_by(:pure_cache, module: module)
  end

  ### Helpers

  # This functions chechs whether the mfargs is pure or not
  @spec lookup(MFArity.mfargs(), stack()) :: boolean()
  defp lookup({_, f, _}, [{module, function, arity} = mfa | stack]) when is_variable(f) do
    with nil <- do_lookup(mfa, stack) do
      pure? = Provider.is_pure({module, function, List.duplicate(nil, arity)}, stack: stack, show: true)
      FunctionRepo.insert(mfa, :pure, pure?)
    end
  end
  defp lookup(mfargs, stack) when is_mfargs(mfargs) do
    mfarity = MFArity.to_mfarity(mfargs)
    with nil <- do_lookup(mfarity, stack) do
      dotted = MFArity.to_dotcall(mfargs)
      case unmeta Codebase.fetch_tria mfargs do
        # No function found
        nil ->
          pure? = Provider.is_pure(mfargs, stack: stack)
          FunctionRepo.insert(mfargs, :pure, pure?)

        # Is a NIF or BIF function
        {:fn, _, [{:"->", _, [_, dot_call(:erlang, :nif_error, _)]}]} ->
          pure? = Provider.is_pure(mfargs, stack: stack)
          FunctionRepo.insert(mfargs, :pure, pure?)

        # Is an Elixir bootstrap
        {:fn, _, [{:"->", _, [_, ^dotted]}]} ->
          pure? = Provider.is_pure(mfargs, stack: stack)
          FunctionRepo.insert(mfargs, :pure, pure?)

        # Anything else, and for this we don't insert the purity check
        # TODO maybe move this in cache
        {:fn, _, clauses}->
          res =
            clauses
            |> Enum.map(fn {:"->", _, [_, body]} -> body end)
            |> check_analyze([mfarity | stack])
          FunctionRepo.insert(mfargs, :pure_cache, res)
      end
    end
  end
  defp lookup({_m, _f, _a}, _stack), do: false

  defp do_lookup({module, function, arity} = mfarity, stack) do
    MFArity.inspect(mfarity, label: :looking_up)
    with(
      false <- :erl_bifs.is_pure(module, function, arity),
      false <- :erl_bifs.is_exit_bif(module, function, arity),

      # Recursive functions are considered pure
      false <- mfarity in stack,
      nil <- FunctionRepo.lookup(mfarity, :pure_cache),
      nil <- FunctionRepo.lookup(mfarity, :pure)
    ) do
      nil
    end
    |> IO.inspect(label: MFArity.to_string mfarity)
  end

  defp fetch_effects() do
    receive do
      {:effect, effect} -> [effect | fetch_effects()]
      after 0 -> []
    end
  end

end
