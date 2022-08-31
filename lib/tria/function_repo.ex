defmodule Tria.FunctionRepo do

  @moduledoc """
  Repository for working with {module, function, arity} traits
  """

  import Tria.Common
  use GenServer

  alias Tria.FunctionRepo.Persister

  @persistent_traits ~w[pure]a

  def insert(mfa, trait, info) do
    trait
    |> ensure_exists()
    |> :ets.insert({arityfy(mfa), info})
  end

  def lookup(mfa, trait) do
    trait
    |> ensure_exists()
    |> :ets.lookup(arityfy mfa)
    |> case do
      [{_, value}] -> value
      _ -> nil
    end
  end

  def lookup_all(mfa) do
    Map.new(traits(), fn trait -> {trait, lookup(mfa, trait)} end)
  end

  def select_by(trait, opts) do
    underscore = :_

    module   = Keyword.get(opts, :module, underscore)
    arity    = Keyword.get(opts, :arity, underscore)
    function = Keyword.get(opts, :function, underscore)
    value    = Keyword.get(opts, :value, underscore)

    trait
    |> ensure_exists()
    |> :ets.match_object({{module, function, arity}, value})
  end

  def traits() do
    GenServer.call(start(), :traits)
  end

  # GenServer callbacks

  def init(_opts) do
    state = %{traits: []}
    {:ok, state}
  end

  def handle_call({:add_table, trait}, _, %{traits: traits} = state) do
    # Here we check on server side, since multiple clients can make
    # calls in the same time
    case :ets.whereis(trait) do
      :undefined ->
        new_table(trait)
        {:reply, :ok, %{state | traits: [trait | traits]}}

      _tid ->
        {:reply, :ok, state}
    end
  end

  def handle_call(:traits, _, %{traits: traits} = state) do
    {:reply, traits, state}
  end

  defp start(opts \\ []) do
    case GenServer.start(__MODULE__, opts, name: __MODULE__) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  # Helpers

  defp ensure_exists(trait) do
    # Here we check on client side just to not make calls beforehand
    with :undefined <- :ets.whereis(trait) do
      # We start FunctionRepo on demand
      GenServer.call(start(), {:add_table, trait})
      :ets.whereis(trait)
    end
  end

  defp new_table(trait) when trait in @persistent_traits do
    new_persistent_table(trait)
  end
  defp new_table(trait), do: new_emphemeral_table(trait)

  defp new_emphemeral_table(trait) do
    :ets.new(trait, [:set, :public, :named_table, {:write_concurrency, true}, {:read_concurrency, true}])
  end

  defp new_persistent_table(trait) do
    filename = file_for_trait(trait)
    if File.exists?(filename) do
      #TODO backup system
      {:ok, ^trait} = :ets.file2tab(filename)
    else
      new_emphemeral_table(trait)
    end
    Persister.add_filetable(filename, trait)
  end

  defp file_for_trait(trait) do
    ~c"#{:code.priv_dir :tria}/#{trait}.ets"
  end

end
