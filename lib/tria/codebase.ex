defmodule Tria.Codebase do

  @moduledoc """
  Module with wrappers around beam_lib and code modules
  which is used to fetch ast in abstract format and other
  module information
  """

  # We explicitly define this function first in the module
  # to avoid garbage in enviroment
  @doc """
  Returns empty enviroment for given module
  """
  @spec empty_env(module() | nil) :: Macro.Env.t()
  def empty_env(module \\ nil) do
    %Macro.Env{__ENV__ | module: module, file: "nofile", line: 0, versioned_vars: %{}, function: nil}
  end

  import Tria.Common

  alias Tria.FunctionRepo
  alias Tria.Translator.Abstract, as: AbstractTranslator

  @typedoc "Name for a chunk in beam lib"
  @type chunk :: atom() | charlist()

  @typedoc "Module.function/arity or list of arguments"
  @type mfargs :: {module(), atom(), arity() | [Tria.t()]}

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
  @spec fetch_attribute(list() | module() | binary(), atom()) :: {:ok, any()} | :error
  def fetch_attribute(abstract_code, name) when is_list(abstract_code) do
    Enum.find_value(abstract_code, :error, fn
      {:attribute, _, ^name, value} -> {:ok, value}
      _ -> false
    end)
  end
  def fetch_attribute(module_or_binary, name) do
    with {:ok, ac} <- fetch_abstract_code(module_or_binary) do
      fetch_attribute(ac, name)
    end
  end

  # Tria tables

  @doc """
  Wrapper to fetch `tria` code for mfa
  """
  @spec fetch_tria(mfargs()) :: Tria.t() | nil
  def fetch_tria(mfa) do
    {module, _name, _arity} = arityfy mfa
    with nil <- FunctionRepo.lookup(mfa, :tria) do
      case fetch_abstract(mfa) do
        nil ->
          nil

        abstract ->
          {:ok, tria, _} = AbstractTranslator.to_tria(abstract, empty_env(module))
          tria = {:fn, [], tria}
          FunctionRepo.insert(mfa, :tria, tria)
          tria
      end
    end
  rescue
    e ->
      IO.inspect(arityfy(mfa), label: :failed_abstract_translation_of)
      reraise e, __STACKTRACE__
  end

  @doc """
  Fetches abstract_code for mfa but also performs caching of already
  request abstract_codes
  """
  @spec fetch_abstract(mfargs()) :: :compile.forms() | nil
  # Guarded because I want to raise ASAP
  def fetch_abstract({module, _, _} = mfa) when is_atom(module) do
    with nil <- FunctionRepo.lookup(mfa, :abstract) do
      prefetch_clauses(module)
      fetch_abstract_from_db(mfa)
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
  defp fetch_abstract_from_db(mfa) do
    FunctionRepo.lookup(mfa, :abstract)
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
