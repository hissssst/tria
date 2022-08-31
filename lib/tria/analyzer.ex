defmodule Tria.Analyzer do

  import Tria.Common

  alias Tria.FunctionRepo
  alias Tria.Translator.Abstract, as: AbstractTranslator

  @moduledoc """
  Purity analyzer for Elixir in-compilation modules and already compiled erlang modules

  #TODO
  """
  
  def fetch_chunks(module) do
    with(
      {^module, bin, _filename} <- :code.get_object_code(module),
      {:ok, {^module, chunks}} <- :beam_lib.chunks(bin, ~w[abstract_code locals exports]a)
    ) do
      {:ok, chunks}
    else
      :error ->
        :error

      error ->
        {:error, error}
    end
  end

  def fetch_chunk(module, chunk_name) do
    with {:ok, chunks} <- fetch_chunks(module), do: Keyword.fetch(chunks, chunk_name)
  end

  def fetch_abstract_code(module) do
    case fetch_chunk(module, :abstract_code) do
      {:ok, {:raw_abstract_v1, ac}} ->
        {:ok, ac}

      _ ->
        :error
    end
  end

  def fetch_tria(mfa) do
    {module, _, _} = arityfy mfa
    case FunctionRepo.lookup(mfa, :tria) do
      nil ->
        case fetch_abstract(mfa) do
          nil ->
            nil

          abstract ->
            {:ok, tria, _} = AbstractTranslator.to_tria(abstract, %Macro.Env{__ENV__ | module: module})
            tria = {:fn, [], tria}
            FunctionRepo.insert(mfa, :tria, tria)
            tria
        end

      tria ->
        tria
    end
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

  def fetch_functions(module) do
    prefetch_clauses(module)

    :abstract
    |> FunctionRepo.select_by(module: module)
    |> Enum.map(fn {{_, f, a}, _} -> {f, a} end)
  end

  defp prefetch_clauses(module) do
    with {:ok, ac} <- fetch_abstract_code(module) do
      for {:function, _anno, name, arity, clauses} <- ac do
        FunctionRepo.insert({module, name, arity}, :abstract, clauses)
      end
    end
  end

end
