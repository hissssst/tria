defmodule Tria.Codebase do

  # We explicitly define this function first in the module
  # to avoid
  defp empty_env(module) do
    %Macro.Env{__ENV__ | module: module, file: "nofile", line: 0, versioned_vars: %{}, function: nil}
  end

  import Tria.Common

  alias Tria.FunctionRepo
  alias Tria.Translator.Abstract, as: AbstractTranslator

  # BEAM chunks

  @doc """
  Fetches chunks from BEAM object code
  """
  def fetch_chunks(module, chunks \\ ~w[abstract_code locals exports]a) do
    with(
      {^module, bin, _filename} <- :code.get_object_code(module),
      {:ok, {^module, chunks}} <- :beam_lib.chunks(bin, chunks)
    ) do
      {:ok, chunks}
    else
      :error ->
        :error

      error ->
        {:error, error}
    end
  end

  @doc """
  Fetches single chunk from BEAM object code
  """
  def fetch_chunk(module, chunk_name) do
    case fetch_chunks(module, [chunk_name]) do
      {:ok, [{^chunk_name, chunk}]} ->
        {:ok, chunk}

      _ ->
        :error
    end
  end

  @doc """
  Fetches abstract code from BEAM object code
  """
  def fetch_abstract_code(module) do
    case fetch_chunk(module, :abstract_code) do
      {:ok, {:raw_abstract_v1, abstract_code}} ->
        {:ok, :erl_expand_records.module(abstract_code, [])}

      _ ->
        :error
    end
  end

  # Tria tables

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

  # Guarded because I want to raise ASAP
  def fetch_abstract({module, _, _} = mfa) when is_atom(module) do
    with nil <- FunctionRepo.lookup(mfa, :abstract) do
      prefetch_clauses(module)
      fetch_abstract_from_db(mfa)
    end
  end

  def fetch_abstract_from_db(mfa) do
    FunctionRepo.lookup(mfa, :abstract)
  end

  defp prefetch_clauses(module) do
    with {:ok, ac} <- fetch_abstract_code(module) do
      for {:function, _anno, name, arity, clauses} <- ac do
        FunctionRepo.insert({module, name, arity}, :abstract, clauses)
      end
    end
  end

  def fetch_functions(module) do
    prefetch_clauses(module)

    :abstract
    |> FunctionRepo.select_by(module: module)
    |> Enum.map(fn {{_, f, a}, _} -> {f, a} end)
  end

  def inspect_fn(module, name) when is_atom(module) do
    {^module, bin, _filename} = :code.get_object_code(module)
    inspect_fn(bin, name)
  end
  def inspect_fn(object_code, name) when is_binary(object_code) do
    {:ok, {_, [{_, {_, ac}}]}} = :beam_lib.chunks(object_code, [:abstract_code])
    for {:function, _, ^name, _, clauses} <- ac do
      inspect_ast {:fn, [], AbstractTranslator.to_tria!(clauses, __ENV__)}
    end

    :ok
  end

  def inspect_fn(module) when is_atom(module) do
    {^module, bin, _filename} = :code.get_object_code(module)
    inspect_fn(bin)
  end
  def inspect_fn(object_code) when is_binary(object_code) do
    {:ok, {_, [{_, {_, ac}}]}} = :beam_lib.chunks(object_code, [:abstract_code])
    for {:function, _, name, _, clauses} <- ac do
      inspect_ast {:fn, [], AbstractTranslator.to_tria!(clauses, __ENV__)}, label: name
    end

    :ok
  end

end
