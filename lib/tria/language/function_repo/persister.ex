defmodule Tria.Language.FunctionRepo.Persister do
  @moduledoc """
  This module is a singleton genserver which periodically persists tables on disk.
  Each table is stored using atomic file lock. This can break on some filesystems.
  """

  use GenServer
  alias Tria.Debug

  @doc """
  New filetable to periodical sync operation
  """
  @spec new_filetable(charlist(), atom(), list()) ::
          {:ok, :overwritten | :picked_up | :created} | {:error, :already_exists}
  def new_filetable(filename, tablename, ets_options) do
    GenServer.call(start(), {:new_filetable, filename, tablename, ets_options})
  end

  @doc """
  Reads the data from filetable
  """
  @spec read_filetable(charlist(), atom()) :: :ok
  def read_filetable(filename, tablename) do
    GenServer.call(start(), {:read_filetable, filename, tablename})
  end

  # Internal

  @spec start(Keyword.t()) :: GenServer.on_start()
  defp start(opts \\ []) do
    case GenServer.start(__MODULE__, opts, name: __MODULE__) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  # GenServer callbacks

  def init(opts) do
    state = %{
      timeout: Keyword.get(opts, :timeout, 100),
      timer: nil,
      filetables: %{}
    }

    send(self(), :tick)
    {:ok, state}
  end

  def handle_info(:tick, state) do
    {:noreply, sync(state)}
  end

  def handle_call({:read_filetable, filename, tablename}, _, state) do
    filename
    |> to_lockfile()
    |> with_filelock(fn ->
      {:ok, ^tablename} = :ets.file2tab(filename)
      tablename
    end)

    {:reply, :ok, state}
  end

  def handle_call(
        {:new_filetable, filename, tablename, ets_opts},
        _,
        %{filetables: filetables} = state
      ) do
    case filetables do
      %{^filename => ^tablename} ->
        {:reply, {:ok, :exists}, state}

      %{^filename => other_tablename} ->
        raise "Table at path #{filename} already exists as #{other_tablename}, but was requested to be started as #{tablename}"

      %{} ->
        if File.exists?(filename) do
          File.rm(to_lockfile(filename))

          case :ets.file2tab(filename) do
            {:error, :badfile} ->
              raise "Table #{tablename} at #{filename} seems to be broken"

            {:ok, ^tablename} ->
              if check_version(tablename) do
                Debug.puts("Picked up existing #{filename} as #{tablename}")
                state = %{state | filetables: Map.put(filetables, filename, tablename)}
                {:reply, {:ok, :picked_up}, state}
              else
                Debug.puts("Table #{tablename} at #{filename} seems to have outdated version")
                IO.puts("Overwriting version for now")
                put_version(tablename)
                state = %{state | filetables: Map.put(filetables, filename, tablename)}
                {:reply, {:ok, :overwritten}, state}
              end

            {:ok, other_tablename} ->
              raise "Table at path #{filename} already exists as #{other_tablename}, but was requested to be started as #{tablename}"
          end
        else
          Debug.puts("Created empty #{filename} as #{tablename}")
          :ets.new(tablename, ets_opts)
          put_version(tablename)
          state = %{state | filetables: Map.put(filetables, filename, tablename)}
          {:reply, {:ok, :created}, state}
        end
    end
  end

  defp sync(%{filetables: filetables, timer: timer, timeout: timeout} = state) do
    timer && Process.cancel_timer(timer)

    Enum.each(filetables, fn {filename, tablename} ->
      filename
      |> to_lockfile()
      |> with_filelock(fn ->
        Debug.puts("Syncing #{tablename} to #{filename}")
        :ok = :ets.tab2file(tablename, filename, sync: true)
      end)
    end)

    %{state | timer: Process.send_after(self(), :tick, timeout)}
  rescue
    exception ->
      IO.puts("Try removing lock file in `priv` dir of Tria")
      reraise exception, __STACKTRACE__
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

  defp check_version(table) do
    case :ets.lookup(table, :__tria_version__) do
      [__tria_version__: version] ->
        version == Tria.version()

      _ ->
        false
    end
  end

  defp put_version(table) do
    :ets.insert(table, __tria_version__: Tria.version())
  end
end
