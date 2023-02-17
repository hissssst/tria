defmodule Tria.Optimizer.Pass do

  @moduledoc """
  A `Pass` is a module which optimization logic.
  Most of the passes can be run several times and this module decouples this logic.
  This module also implements timeout based optimization interruption
  """

  @type opts :: Keyword.t()

  @type state :: any()

  @type reason :: atom()

  @type t :: module()

  @callback begin(Tria.t(), opts()) :: {:ok, state()} | {:error, reason}

  @callback run_once(state(), opts()) :: {:ok, state()} | {:error, reason()}

  @callback finish(state(), opts()) :: {:ok, Tria.t()} | {:error, reason()}

  @optional_callbacks [begin: 2, finish: 2]

  @spec default_timeout() :: pos_integer()
  def default_timeout do
    :persistent_term.get(:tria_optimizer_pass_timeout, 5000)
  end

  @spec run_until(t(), Tria.t(), opts()) :: {:ok, Tria.t()} | {:error, reason()}
  def run_until(pass, ast, opts \\ []) do
    timer = Process.send_after(self(), :stop, Keyword.get(opts, :timeout, default_timeout()))

    try do
      with {:ok, state} <- pass.begin(ast, opts) do
        pass
        |> run_once(state, opts)
        |> pass.finish(opts)
      end
    after
      Process.cancel_timer(timer)
      receive do
        :stop -> :ok
        after 0 -> :ok
      end
    end
  end

  defp run_once(pass, state, opts) do
    receive do
      :stop -> state
      after 0 ->
        case pass.run_once(state, opts) do
          {:error, :nothing_to_optimize} ->
            state

          {:ok, new_state} ->
            run_once(pass, new_state, opts)
        end
    end
  end

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      def run_while(ast, opts \\ []) do
        {:ok, result} = unquote(__MODULE__).run_until(__MODULE__, ast, opts)
        result
      end

      defoverridable run_while: 2, run_while: 1
    end
  end

end
