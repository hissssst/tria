defmodule Tria.Compiler.Annotations do

  @moduledoc """
  Helper for working with tria annotations.
  An annotation is an ability for the user to mark the traits of functions.

  For example:

  ```elixir
  defmodule X do
    use Tria

    @tria safe: true
    def f(x), do: x + 1
  end
  ```
  """

  alias Tria.Compiler
  alias Tria.Language.FunctionRepo
  alias Tria.Language.MFArity
  import Tria.Language.MFArity, only: [is_mfarity: 1]

  @typedoc """
  `:safe` (boolean) -- defines whether function is safe or not
  `:pure` (boolean) -- defines whether function is pure or not
  `:optimize` (boolean) -- defines whether to optimize the function or not
  """
  @type annotation :: {:safe, boolean()}
  | {:pure, boolean()}
  | {:optimize, boolean()}

  @type t :: [annotation()]

  @type error :: {:unknown_key, atom()} | {:kind_not_supported, atom()}

  defguard valid_annotation?(key, value) when key in ~w[pure safe optimize]a and is_boolean(value)

  @spec valid_annotations?(t()) :: boolean()
  def valid_annotations?(annotations) do
    Enum.all?(annotations, fn {key, value} -> valid_annotation?(key, value) end)
  end

  @doc """
  Puts a list of lists of annotations into storages
  This function configures annotations per function
  """
  @spec put_annotations([{Compiler.signature() | MFArity.mfarity(), t()}]) :: [error()]
  def put_annotations(annotations) do
    Enum.flat_map(annotations, fn {signature, annotations} ->
      put_annotations(signature, annotations)
    end)
  end

  @doc """
  Puts several annotations for passed mfarity or signature
  """
  @spec put_annotations(MFArity.mfarity() | Compiler.signature(), t()) :: [error()]
  def put_annotations(mfarity_or_signature, annotations) do
    Enum.flat_map(annotations, fn {key, value} ->
      case put_annotation(mfarity_or_signature, key, value) do
        :ok -> []
        {:error, error} -> [error]
      end
    end)
  end

  @doc """
  Puts passed annotation for mfarity or signature
  """
  @spec put_annotation(MFArity.mfarity() | Compiler.signature(), atom(), any()) :: :ok | {:error, {:kind_not_supported, atom()}}
  def put_annotation({module, kind, name, arity}, key, value) when kind in ~w[def defp]a do
    put_annotation({module, name, arity}, key, value)
  end
  def put_annotation({_module, kind, _name, _arity}, _key, _value) do
    {:error, {:kind_not_supported, kind}}
  end
  def put_annotation(mfarity, key, value) when is_mfarity(mfarity) and key in ~w[pure safe]a do
    FunctionRepo.insert(mfarity, :"#{key}_cache", value)
    :ok
  end
  def put_annotation(mfarity, key, value) when is_mfarity(mfarity) and key in ~w[optimize]a do
    FunctionRepo.insert(mfarity, key, value)
    :ok
  end
  def put_annotation(mfarity, key, _value) when is_mfarity(mfarity) do
    {:error, {:unknown_key, key}}
  end

end
