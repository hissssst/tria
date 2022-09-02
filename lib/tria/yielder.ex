defmodule Tria.Yielder do

  @moduledoc """
  It's like a task, but it recursively runs function against arguments
  and sends results to the caller, until finished
  """

  use GenServer

  @type option :: {:timeout, pos_integer()}

  @spec run(any(), (any() -> {:cont | :halt, any()}), [option]) :: {:ok, any()} | :error | GenServer.on_start()
  def run(arg, func, opts) do
    ref = Process.send_after(self(), :stop, Keyword.fetch!(opts, :timeout))
    with {:ok, pid} <- GenServer.start_link(__MODULE__, {arg, func, self(), ref}) do
      await(pid, ref, :error)
    end
  end

  # GenServer callbacks

  def init({arg, func, caller, ref}) do
    state = %{
      arg: arg,
      func: func,
      caller: caller,
      ref: ref
    }
    {:ok, state, {:continue, :work}}
  end

  def handle_continue(:work, state) do
    IO.inspect :working
    case state.func.(state.arg) do
      {:cont, res} ->
        state = yield(%{state | arg: res})
        {:noreply, state, {:continue, :work}}

      {:halt, res} ->
        state = %{state | arg: res}
        {:stop, :normal, state}
    end
    |> IO.inspect(label: :work_result)
  end

  def terminate(_reason, %{arg: arg, caller: caller, ref: ref}) do
    send(caller, {:stop, ref, arg})
  end

  # Helpers

  defp await(pid, ref, latest) do
    receive do
      # Timeout
      :stop ->
        Process.unlink(pid)
        Process.exit(pid, :kill)
        Process.cancel_timer(ref)
        latest

      # Yielded result
      {:yield, ^ref, latest} ->
        await(pid, ref, {:ok, latest})

      # Stopped on itself
      {:stop, ^ref, latest} ->
        Process.cancel_timer(ref)
        {:ok, latest}
    end
  end

  defp yield(%{arg: arg, caller: caller, ref: ref} = state) do
    send(caller, {:yield, ref, arg})
    state
  end

end
