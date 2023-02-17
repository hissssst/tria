defmodule Tria.Language.Codebase do

  @moduledoc """
  Module with wrappers around beam_lib and code modules
  which is used to fetch ast in abstract format and other
  module information
  """

  # We explicitly define this function first in the module
  # to avoid garbage in enviroment

  import Tria.Language
  import Tria.Language.MFArity, only: [is_mfarity: 1]

  alias Tria.Language.MFArity
  alias Tria.Language.FunctionRepo
  alias Tria.Compiler.AbstractTranslator

  @typedoc "Name for a chunk in beam lib"
  @type chunk :: atom() | charlist()

  @type abstract_code :: list()

  # BEAM chunks

  @doc """
  Fetches chunks from BEAM object code
  """
  @spec fetch_chunks(module() | binary(), [chunk()]) :: {:ok, [{chunk(), any()}]} | {:error, reason :: any()}
  def fetch_chunks(module, chunks \\ ~w[abstract_code locals exports]a)
  def fetch_chunks(module, chunks) when is_atom(module) do
    with {^module, bin, _filename} <- :code.get_object_code(module) do
      fetch_chunks(bin, chunks)
    end
  end
  def fetch_chunks(bin, chunks) when is_binary(bin) do
    case :beam_lib.chunks(bin, chunks) do
      {:ok, {_module, chunks}} -> {:ok, chunks}
      {:error, :beam_lib, error} -> {:error, error}
    end
  end

  @doc """
  Fetches single chunk from BEAM object code
  """
  @spec fetch_chunk(module() | binary(), chunk()) :: {:ok, any()} | {:error, reason :: any()}
  def fetch_chunk(module_or_binary, chunk_name) do
    case fetch_chunks(module_or_binary, [chunk_name]) do
      {:ok, [{^chunk_name, chunk}]} -> {:ok, chunk}
      _ -> :error
    end
  end

  @doc """
  Fetches abstract code from BEAM object code
  """
  @spec fetch_abstract_code(module() | binary()) :: {:ok, :compile.forms()} | {:error, reason :: any()}
  def fetch_abstract_code(module_or_binary) do
    case fetch_chunk(module_or_binary, :abstract_code) do
      {:ok, {:raw_abstract_v1, abstract_code}} ->
        {:ok, :erl_expand_records.module(abstract_code, [])}

      _ ->
        :error
    end
  end

  @doc """
  Fetches attribute value from BEAM's abstract code
  """
  @spec fetch_attribute(list() | module() | binary(), atom()) :: {:ok, [any()]} | :error
  def fetch_attribute(abstract_code, name) when is_list(abstract_code) do
    values =
      for {:attribute, _, ^name, value} <- abstract_code do
        value
      end

    case values do
      [] -> :error
      values -> {:ok, values}
    end
  end
  def fetch_attribute(module_or_binary, name) do
    with {:ok, ac} <- fetch_abstract_code(module_or_binary) do
      fetch_attribute(ac, name)
    end
  end

  @spec fetch_attribute(list(), [atom()]) :: %{atom() => any()}
  def fetch_attributes(abstract_code, names) when is_list(abstract_code) do
    acc = Map.new(names, fn name -> {name, []} end)
    Enum.reduce(abstract_code, acc, fn
      {:attribute, _, name, value}, acc ->
        if name in names do
          Map.update!(acc, name, &[value | &1])
        else
          acc
        end

      _, acc ->
        acc
    end)
  end

  # Tria tables

  @doc """
  Wrapper to fetch `tria` code for mfa
  """
  @spec fetch_tria(MFArity.mfarity()) :: Tria.t() | nil
  def fetch_tria({module, _, _} = mfarity) when is_mfarity(mfarity) do
    with nil <- FunctionRepo.lookup(mfarity, :tria) do
      case fetch_abstract(mfarity) do
        nil -> nil
        abstract ->
          clauses = AbstractTranslator.to_tria!(abstract, env: empty_env(module))
          FunctionRepo.insert(mfarity, :tria, {:fn, [], clauses})
      end
    end
  rescue
    e ->
      MFArity.inspect(mfarity, label: :failed_abstract_translation_of)
      reraise e, __STACKTRACE__
  end

  @spec fetch_tria_functions(abstract_code()) :: {:ok, definitions :: list()} | :error
  def fetch_tria_functions(abstract_code) when is_list(abstract_code) do
    {module, exports} =
      Enum.reduce(abstract_code, {nil, nil}, fn
        {:attribute, _, :module, module}, {_, export} -> {module, export}
        {:attribute, _, :export, export}, {module, _} -> {module, export}
        _, acc -> acc
      end)

    definitions =
      for {:function, _anno, name, arity, clauses} when name != :__info__ <- abstract_code do
        clauses = AbstractTranslator.to_tria!(clauses, env: empty_env(module))
        kind = if {name, arity} in exports, do: :def, else: :defp
        {{module, kind, name, arity}, clauses}
      end

    {:ok, definitions}
  end
  def fetch_tria_functions(module_or_binary) do
    with {:ok, abstract_code} <- fetch_abstract_code(module_or_binary) do
      fetch_tria_functions(abstract_code)
    end
  end

  @doc """
  Lists mfarities of the passed module
  """
  @spec list_functions(abstract_code() | binary() | module()) :: [MFArity.mfarity()]
  def list_functions(abstract_code) when is_list(abstract_code) do
    module =
      Enum.find_value(abstract_code, fn
        {:attribute, _, :module, module} -> module
        _ -> false
      end)

    for {:function, _anno, name, arity, _clauses} <- abstract_code do
      {module, name, arity}
    end
  end
  def list_functions(module_or_binary) do
    with {:ok, ac} <- fetch_abstract_code(module_or_binary) do
      list_functions(ac)
    end
  end

  @doc """
  Fetches tria, but without top-level `fn`, only the list of bodies
  """
  @spec fetch_tria_bodies(MFArity.mfarity()) :: [Tria.t()] | nil
  def fetch_tria_bodies(mfarity) do
    with {:fn, _, clauses} <- fetch_tria(mfarity) do
      :lists.map(fn {:"->", _, [_, body]} -> body end, clauses)
    end
  end

  @doc """
  Fetches abstract_code for mfa but also performs caching of already
  request abstract_codes
  """
  @spec fetch_abstract(MFArity.mfarity()) :: :compile.forms() | nil
  # Guarded because I want to raise ASAP
  def fetch_abstract({module, _, _} = mfarity) when is_mfarity(mfarity) do
    with nil <- FunctionRepo.lookup(mfarity, :abstract) do
      prefetch_clauses(module)
      fetch_abstract_from_db(mfarity)
    end
  end

  @doc """
  Returns a list of functions from abstract_code for passed `module`
  """
  @spec fetch_functions(module()) :: [{atom(), arity()}]
  def fetch_functions(module) do
    prefetch_clauses(module)

    :abstract
    |> FunctionRepo.select_by(module: module)
    |> Enum.map(fn {{_, f, a}, _} -> {f, a} end)
  end

  ### Functions for debugging

  @doc """
  Inspects given Module.function
  """
  @spec inspect_fn(module() | binary(), atom()) :: :ok
  def inspect_fn(module, name) when is_atom(module) do
    {^module, bin, _filename} = :code.get_object_code(module)
    inspect_fn(bin, name)
  end
  def inspect_fn(object_code, name) when is_binary(object_code) do
    {:ok, {_, [{_, {_, ac}}]}} = :beam_lib.chunks(object_code, [:abstract_code])
    for {:function, _, ^name, _, clauses} <- ac do
      inspect_ast({:fn, [], AbstractTranslator.to_tria!(clauses, __ENV__)}, with_contexts: true)
    end

    :ok
  end

  @doc """
  Inspects given `module`
  """
  @spec inspect_fn(module() | binary()) :: :ok
  def inspect_fn(module) when is_atom(module) do
    {^module, bin, _filename} = :code.get_object_code(module)
    inspect_fn(bin)
  end
  def inspect_fn(object_code) when is_binary(object_code) do
    {:ok, {_, [{_, {_, ac}}]}} = :beam_lib.chunks(object_code, [:abstract_code])
    for {:function, _, name, _, clauses} <- ac do
      inspect_ast({:fn, [], AbstractTranslator.to_tria!(clauses, __ENV__)}, with_contexts: true, label: name)
    end

    :ok
  end

  ### Helpers

  # Fetches straight from the cache
  defp fetch_abstract_from_db(mfarity) do
    FunctionRepo.lookup(mfarity, :abstract)
  end

  # Warms up the cache
  defp prefetch_clauses(module) do
    with {:ok, ac} <- fetch_abstract_code(module) do
      for {:function, _anno, name, arity, clauses} <- ac do
        FunctionRepo.insert({module, name, arity}, :abstract, clauses)
      end
    end
  end

end
