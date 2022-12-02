defmodule Tria.Language.FunctionRepo.Persister do
  use GenServer

  def add_filetable(filename, tablename) when is_list(filename) do
    case start(filetables: %{filename => tablename}) do
      {:ok, _} ->
        {:ok, :started}

      {:error, {:already_started, pid}} ->
        GenServer.call(pid, {:add_filetable, filename, tablename})
    end
  end
  def add_filetable(filename, _) when is_binary(filename) do
    raise "Filename #{inspect filename} must be a charlist, sorry"
  end

  # Internal

  defp start(opts) do
    GenServer.start(__MODULE__, opts, name: __MODULE__)
  end

  # GenServer callbacks

  def init(opts) do
    state = %{
      timeout: Keyword.get(opts, :timeout, 100),
      timer: nil,
      filetables: Keyword.get(opts, :filetables, %{})
    }

    send(self(), :tick)
    {:ok, state}
  end

  def handle_info(:tick, state) do
    {:noreply, sync(state)}
  end

  def handle_call({:add_filetable, filename, tablename}, _, %{filetables: filetables} = state) do
    case filetables do
      %{^filename => ^tablename} ->
        {:reply, {:ok, :already_exists}, state}

      %{^filename => _} ->
        {:reply, {:error, :already_exists}, state}

      %{} ->
        state = %{state | filetables: Map.put(filetables, filename, tablename)}
        {:reply, {:ok, :new}, sync(state)}
    end
  end

  defp sync(%{filetables: filetables, timer: timer, timeout: timeout} = state) do
    timer && Process.cancel_timer(timer)
    Enum.each(filetables, fn {filename, tablename} ->
      :ok = :ets.tab2file(tablename, filename, sync: true)
    end)
    %{state | timer: Process.send_after(self(), :tick, timeout)}
  end

end
