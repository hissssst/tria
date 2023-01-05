defmodule Tria.Optimizer.Pass do

  @type opts :: Keyword.t()

  @type state :: any()

  @type reason :: atom()

  @callback begin(Tria.t(), opts()) :: {:ok, state()} | {:error, reason}

  @callback run_once(state(), opts()) :: {:ok, state()} | {:error, reason()}

  @callback finish(state(), opts()) :: {:ok, Tria.t()} | {:error, reason()}

  def run_until(pass, ast, opts \\ []) do
    timer = Process.send_after(self(), :stop, Keyword.get(opts, :timeout, 5000))

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
