defmodule Tria.Fundb do

  import Tria.Common

  def insert(mfa, key, info) do
    :ets.insert(ensure_exists(), {{mfa(mfa), key}, info})
  end

  def lookup(mfa, key) do
    ensure_exists()
    |> :ets.lookup({mfa(mfa), key})
    |> case do
      [{_, value}] -> value
      _ -> nil
    end
  end

  defp ensure_exists do
    with :undefined <- :ets.whereis(__MODULE__) do
      :ets.new(__MODULE__, [:set, :public, :named_table, {:write_concurrency, true}, {:read_concurrency, true}])
    end

    __MODULE__
  end

  defp mfa({{:".", _, [module, function]}, _, arguments}) do
    mfa {unalias(module), function, arguments}
  end

  defp mfa({module, function, arguments}) when is_list(arguments) do
    {module, function, length(arguments)}
  end

  defp mfa({module, function, arguments}) when is_integer(arguments) do
    {module, function, arguments}
  end
 
end
