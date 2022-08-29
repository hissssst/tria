defmodule Tria.Fundb do

  import Tria.Common

  def insert(mfa, key, info) do
    key
    |> ensure_exists()
    |> :ets.insert({arityfy(mfa), info})
  end

  def lookup(mfa, key) do
    key
    |> ensure_exists()
    |> :ets.lookup(arityfy mfa)
    |> case do
      [{_, value}] -> value
      _ -> nil
    end
  end

  def at(func) do
    Agent.get(start(), fn _ -> func.() end, :infinity)
  end

  def select_by(key, opts) do
    underscore = :_

    module   = Keyword.get(opts, :module, underscore)
    arity    = Keyword.get(opts, :arity, underscore)
    function = Keyword.get(opts, :function, underscore)
    value    = Keyword.get(opts, :value, underscore)

    key
    |> ensure_exists()
    |> :ets.match_object({{module, function, arity}, value})
  end

  defp saveloop(key, filename) do
    Process.sleep(50)
    :ets.tab2file(key, filename)
    saveloop(key, filename)
  end

  defp ensure_exists(key) do
    with :undefined <- :ets.whereis(key) do
      Agent.get(start(), fn _ ->
        with :undefined <- :ets.whereis(key) do
          if key == :pure do
            filename = ~c"#{:code.priv_dir :tria}/#{key}.ets"
            IO.inspect filename, label: :filename
            if File.exists?(filename) do
              IO.inspect :ets.file2tab(filename), label: :res
              IO.inspect :ets.tab2list(key), label: key
            else
              :ets.new(key, [:set, :public, :named_table, {:write_concurrency, true}, {:read_concurrency, true}])
              :ets.tab2file(key, filename)
            end
            spawn fn -> saveloop(key, filename) end
          else
            :ets.new(key, [:set, :public, :named_table, {:write_concurrency, true}, {:read_concurrency, true}])
          end
        end
      end)
    end

    key
  end

  defp start() do
    case Agent.start(fn -> [] end, name: __MODULE__) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

end
