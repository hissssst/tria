defmodule Tria.Language.FunctionRepo.Persister do
  # This module is a singleton genserver which
  # periodically persists tables on disk

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
      filename
      |> to_lockfile()
      |> with_filelock(fn ->
        :ok = :ets.tab2file(tablename, filename, sync: true)
      end)
    end)
    %{state | timer: Process.send_after(self(), :tick, timeout)}
  end

  defp with_filelock(lockfile, func) do
    maybe_delete_old(lockfile)
    salt = :rand.bytes(16)
    with(
      :ok <- File.write(lockfile, salt, [:exclusive, :raw]),
      {:ok, ^salt} <- File.read(lockfile)
    ) do
      try do
        func.()
      after
        File.rm(lockfile)
      end
    else
      _ ->
        Process.sleep(:rand.uniform(40) + 10)
        with_filelock(lockfile, func)
    end
  end

  defp to_lockfile(filename) do
    base = Path.basename(filename)
    dirname = Path.dirname(filename)
    Path.join(dirname, ".#{base}.lock")
  end

  defp maybe_delete_old(lockfile) do
    with {:ok, %File.Stat{ctime: ctime}} <- File.stat(lockfile) do
      timestamp = NaiveDateTime.from_erl!(ctime)
      diff = NaiveDateTime.diff(NaiveDateTime.utc_now(), timestamp)
      if diff >= 2 do
        File.rm!(lockfile)
      end
    end
  end

end
