defmodule Tria.Language.Meta do

  @moduledoc """
  Module for working with metadata
  """

  import Tria.Language, warn: false

  @type option :: {atom(), any()}
  @type t :: [option()]

  @doc """
  Recursively removes metadata from AST
  """
  @spec unmeta(Tria.t()) :: Tria.t()
  def unmeta([head | tail]) do
    [unmeta(head) | unmeta(tail)]
  end
  def unmeta({left, right}), do: {unmeta(left), unmeta(right)}
  def unmeta({left, _, right}), do: {unmeta(left), [], unmeta(right)}
  def unmeta(other), do: other

  @spec with_meta(Tria.t(), t()) :: Tria.t()
  def with_meta({left, _, right}, meta) do
    {left, meta, right}
  end
  def with_meta(other, _), do: other

  @spec meta_of(Tria.t()) :: t()
  def meta_of({_, meta, _}), do: meta
  def meta_of(_), do: []

end
