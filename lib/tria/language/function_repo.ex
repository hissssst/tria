defmodule Tria.Language.FunctionRepo do
  @moduledoc """
  Repository for working with {module, function, arity} traits
  It lazily initializes trait tables. Each trait has it's own table.
  Some tables are persisted to the disk to acheive maximum efficiency
  """

  use GenServer

  alias Tria.Language.MFArity
  alias Tria.Language.FunctionRepo.Persister

  @persistent_traits ~w[pure pure_cache safe_cache]a

  @typedoc "Traits can be only atoms"
  @type trait :: atom()

  @typedoc "Anything can be an entry"
  @type entry :: any()

  @typedoc "Monadic-alike maybe"
  @type maybe(something) :: something | nil

  @typedoc "List of options for `select_*` functions"
  @type select_options() :: [select_option()] | []

  @typedoc "Each options defines the upon which to select"
  @type select_option ::
          {:module, module()}
          | {:arity, arity()}
          | {:function, atom()}
          | {:entry, entry()}

  @doc "Inserts the entry for the specified mfa and trait"
  @spec insert(mfa(), trait(), entry()) :: entry()
  def insert(mfa, trait, entry) do
    trait
    |> ensure_exists()
    |> :ets.insert({MFArity.to_mfarity(mfa), entry})

    entry
  end

  @doc "Looks up the entry for the specified mfa and trait in either style"
  @spec fetch(mfa(), trait()) :: {:ok, entry()} | :error
  def fetch(mfa, trait) do
    trait
    |> ensure_exists()
    |> :ets.lookup(MFArity.to_mfarity(mfa))
    |> case do
      [{_, entry}] -> {:ok, entry}
      _ -> :error
    end
  end

  @doc "Looks up the entry for the specified mfa and trait in maybe style"
  @spec lookup(mfa(), trait(), default) :: entry | default
        when default: any()
  def lookup(mfa, trait, default \\ nil) do
    trait
    |> ensure_exists()
    |> :ets.lookup(MFArity.to_mfarity(mfa))
    |> case do
      [{_, entry}] -> entry
      _ -> default
    end
  end

  @doc "Looks up all entries for the specified mfa"
  @spec lookup_all(mfa()) :: %{trait() => maybe(entry())}
  def lookup_all(mfa), do: lookup_all(mfa, traits())

  @doc "Looks up all entries for the specified traits and mfa"
  @spec lookup_all(mfa(), [trait()]) :: %{trait() => maybe(entry())}
  def lookup_all(mfa, traits_to_lookup) do
    Map.new(traits_to_lookup, fn trait -> {trait, lookup(mfa, trait)} end)
  end

  @doc "Filters the traits table with the specified select options"
  @spec select_by(trait(), select_options()) :: [{mfa(), entry()}]
  def select_by(trait, opts) do
    trait
    |> ensure_exists()
    |> :ets.match_object(opts_to_spec(opts))
  end

  @spec exists?(trait(), select_options()) :: boolean()
  def exists?(trait, opts) do
    trait
    |> ensure_exists()
    |> :ets.match_object(opts_to_spec(opts), 1)
    |> Kernel.!=(:"$end_of_table")
  end

  @doc "Filters all trait tables with the specified select options"
  @spec select_all_by([trait()] | nil, select_options()) :: [{mfa(), %{trait() => entry()}}]
  def select_all_by(traits_to_select \\ nil, opts) do
    spec = opts_to_spec(opts)

    traits_to_select = with nil <- traits_to_select, do: traits()

    traits_to_select
    |> Enum.reduce(%{}, fn trait, acc ->
      trait
      |> ensure_exists()
      |> :ets.match_object(spec)
      |> Enum.reduce(acc, fn {mfa, entry}, acc ->
        case acc do
          %{^mfa => entrys} ->
            %{acc | mfa => Map.put(entrys, trait, entry)}

          _ ->
            Map.put(acc, mfa, %{trait => entry})
        end
      end)
    end)
    |> Map.to_list()
  end

  @doc "Returns a list of currently active and persistent traits"
  @spec traits() :: [trait()]
  def traits() do
    GenServer.call(start(), :traits) ++ @persistent_traits
  end

  @spec remove_by(trait(), select_options()) :: :ok
  def remove_by(trait, select_opts) do
    trait
    |> select_by(select_opts)
    |> Enum.each(fn {key, _} -> :ets.delete(trait, key) end)
  end

  @spec exists_any?([trait()], select_options()) :: boolean()
  def exists_any?(traits, select_opts) do
    do_exists_any?(traits, opts_to_spec(select_opts))
  end

  defp do_exists_any?([], _), do: false

  defp do_exists_any?([trait | traits], spec) do
    trait
    |> ensure_exists()
    |> :ets.match_object(spec, 1)
    |> case do
      :"$end_of_table" -> do_exists_any?(traits, spec)
      _ -> true
    end
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

  @table_options [
    :set,
    :public,
    :named_table,
    {:write_concurrency, true},
    {:read_concurrency, true}
  ]

  defp new_emphemeral_table(trait) do
    :ets.new(trait, @table_options)
  end

  defp new_persistent_table(trait) do
    filename = file_for_trait(trait)
    Persister.new_filetable(filename, trait, @table_options)
  end

  defp file_for_trait(trait) do
    ~c"#{:code.priv_dir(:tria)}/#{trait}.ets"
  end

  # Creates match spec from opts for `select_*` functions
  defp opts_to_spec(opts) do
    underscore = :_
    module = Keyword.get(opts, :module, underscore)
    arity = Keyword.get(opts, :arity, underscore)
    function = Keyword.get(opts, :function, underscore)
    entry = Keyword.get(opts, :entry, underscore)
    {{module, function, arity}, entry}
  end
end
