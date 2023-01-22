defmodule Tria.Language.Analyzer.Purity do

  @moduledoc """
  Module which analyzes AST for Purity

  # TODO

  * ternary logic
  * handle closures (like in Enum.map)
  """

  import Tria.Language
  import Tria.Language.MFArity, only: :macros

  alias Tria.Debug.Tracer
  alias Tria.Language.MFArity
  alias Tria.Language.Interpreter
  alias Tria.Language.{Codebase, FunctionRepo}
  alias Tria.Language.Analyzer.Provider

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
  def check_analyze(ast, stack \\ []) do
    prewalk(ast, fn
      dot_call(m, f, a) ->
        mfa = {m, f, a}
        if lookup(mfa, stack), do: mfa, else: throw(false)

      # Even if function creates fn with impure body, this function is still pure
      {:fn, _, _} ->
        nil

      # Receive is always impure, even just sleeping
      {:receive, _, _} ->
        throw false

      # Optimization to not waste time on analysis of patterns
      {op, _, [_, right]} when op in ~w[-> = <-]a ->
        if check_analyze(right, stack), do: nil, else: throw(false)

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

  # This functions checks whether the mfargs is pure or not
  @spec lookup(MFArity.mfargs(), stack()) :: boolean()
  defp lookup({_, :__impl__, [:target]}, _), do: true
  defp lookup({_, f, _}, [{module, function, arity} = mfarity | stack]) when is_variable(f) do
    with nil <- do_lookup(mfarity, stack) do
      pure? = ask_provider({module, function, List.duplicate(nil, arity)}, stack)
      FunctionRepo.insert(mfarity, :pure, pure?)
    end
  end
  defp lookup(mfargs, stack) when is_mfargs(mfargs) do
    {module, function, arity} = mfarity = MFArity.to_mfarity(mfargs)
    with nil <- do_lookup(mfarity, stack) do
      case Codebase.fetch_tria_bodies mfarity do
        # No function found
        nil ->
          pure? = ask_provider(mfargs, stack)
          FunctionRepo.insert(mfarity, :pure, pure?)

        # Is a NIF or BIF function
        [dot_call(:erlang, :nif_error, _)] ->
          pure? = ask_provider(mfargs, stack)
          FunctionRepo.insert(mfarity, :pure, pure?)

        # Single clause function which calls itself is recursively defined
        # So we better ask
        [dot_call(^module, ^function, args)] when length(args) == arity ->
          pure? = ask_provider(mfargs, stack)
          FunctionRepo.insert(mfarity, :pure, pure?)

        # Anything else, and for this we don't insert the purity check
        bodies ->
          pure? = check_analyze(bodies, [mfarity | stack])
          FunctionRepo.insert(mfarity, :pure_cache, pure?)
      end
    end
  end
  defp lookup({_m, _f, _a}, _stack), do: false

  defp do_lookup({module, function, arity} = mfarity, stack) do
    mfarity
    |> MFArity.to_string()
    |> Tracer.tag(label: :purity_lookup, key: mfarity)

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
    |> Tracer.tag(label: :purity_lookup_result, key: mfarity)
  end

  defp fetch_effects() do
    receive do
      {:effect, effect} -> [effect | fetch_effects()]
      after 0 -> []
    end
  end

  defp ask_provider(mfarity, stack) do
    Provider.decide(:pure, mfarity, stack: stack, show: true)
  end

end
