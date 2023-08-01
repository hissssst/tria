defmodule Tria.Language.Beam do

  @moduledoc """
  Module with wrappers around beam_lib and code modules
  which is used to fetch ast in abstract format and other
  module information

  Works only with modules currently loaded in runtime
  """

  # We explicitly define this function first in the module
  # to avoid garbage in enviroment

  import Tria.Language
  import Tria.Language.MFArity, only: [is_mfarity: 3]

  alias Tria.Debug
  alias Tria.Language.MFArity
  alias Tria.Compiler.AbstractTranslator

  @typedoc "Name for a chunk in beam lib"
  @type chunk :: atom() | charlist()

  @type abstract_code :: :compile.forms()

  @type object_code :: binary()

  # BEAM chunks

  @doc """
  Extracts module from BEAM's object_code
  """
  @spec module(object_code()) :: atom()
  def module(object_code) do
    :beam_lib.info(object_code)[:module]
  end

  @doc """
  Stores BEAM in ETS
  """
  @spec store_object_code(module(), object_code()) :: :ok
  def store_object_code(module, object_code) do
    storage = ensure_storage()
    :ets.insert(storage, {module, object_code})
    :ok
  end

  @doc """
  Fetches obeject_code from module name
  """
  @spec object_code(module()) :: {:ok, object_code()} | :error
  def object_code(module) do
    case :ets.lookup(ensure_storage(), module) do
      [] ->
        case :code.get_object_code(module) do
          {^module, object_code, _filename} ->
            {:ok, object_code}

          _ ->
            :error
        end

      [{^module, object_code}] ->
        {:ok, object_code}
    end
  end

  @doc """
  Fetches obeject_code from module name in raising manner
  """
  @spec object_code!(module()) :: object_code()
  def object_code!(module) do
    case object_code(module) do
      {:ok, bin} ->
        bin

      :error ->
        raise "Object code for module #{inspect module} not found"
    end
  end

  @doc """
  Fetches chunks from BEAM object code
  """
  @spec chunks(object_code(), [chunk()]) :: {:ok, [{chunk(), any()}]} | {:error, reason :: any()}
  def chunks(binary, chunks \\ ~w[abstract_code locals exports]a) do
    case :beam_lib.chunks(binary, chunks) do
      {:ok, {_module, chunks}} -> {:ok, chunks}
      {:error, :beam_lib, error} -> {:error, error}
    end
  end

  @doc """
  Fetches single chunk from BEAM object code
  """
  @spec chunk(object_code(), chunk()) :: {:ok, any()} | :error
  def chunk(binary, chunk_name) do
    case chunks(binary, [chunk_name]) do
      {:ok, [{^chunk_name, chunk}]} -> {:ok, chunk}
      _ -> :error
    end
  end

  @doc """
  Fetches abstract code from BEAM object code
  """
  @spec abstract_code(object_code()) :: {:ok, abstract_code()} | :error
  def abstract_code(binary) do
    case chunk(binary, :abstract_code) do
      {:ok, {:raw_abstract_v1, abstract_code}} ->
        {:ok, :erl_expand_records.module(abstract_code, [])}

      _ ->
        :error
    end
  end

  @doc """
  Fetches abstract code from BEAM object code
  """
  @spec abstract_code!(object_code()) :: abstract_code()
  def abstract_code!(binary) do
    case chunk(binary, :abstract_code) do
      {:ok, {:raw_abstract_v1, abstract_code}} ->
        :erl_expand_records.module(abstract_code, [])

      _ ->
        raise "Abstract code chunk is not present in passed binary"
    end
  end

  @doc """
  Fetches attribute value from BEAM's abstract code
  """
  @spec attribute(abstract_code(), atom()) :: {:ok, [any()]} | :error
  def attribute(abstract_code, name) do
    values =
      for {:attribute, _, ^name, value} <- abstract_code do
        value
      end

    case values do
      [] -> :error
      values -> {:ok, values}
    end
  end

  @doc """
  Lists all attributes in the module
  """
  @spec attributes(abstract_code()) :: %{atom() => [any()]}
  def attributes(abstract_code) do
    Enum.reduce(abstract_code, %{}, fn
      {:attribute, _, name, value}, acc ->
        Map.update(acc, name, [value], &[value | &1])

      _, acc ->
        acc
    end)
  end

  @doc """
  Lists specified attributes in the module
  """
  @spec attributes(abstract_code(), [atom()]) :: %{atom() => [any()]}
  def attributes(abstract_code, names) do
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
  @spec tria(abstract_code(), MFArity.mfarity()) :: Tria.t() | nil
  def tria(abstract_code, {module, name, arity}) when is_mfarity(module, name, arity) do
    case abstract(abstract_code, name, arity) do
      nil -> nil
      abstract ->
        clauses = AbstractTranslator.to_tria!(abstract, env: empty_env(module))
        {:fn, [], clauses}
    end
  end

  @spec tria_functions(abstract_code()) :: [Tria.Compiler.definition()]
  def tria_functions(abstract_code) when is_list(abstract_code) do
    {module, exports} =
      Enum.reduce(abstract_code, {nil, nil}, fn
        {:attribute, _, :module, module}, {_, export} -> {module, export}
        {:attribute, _, :export, export}, {module, _} -> {module, export}
        _, acc -> acc
      end)

    for {:function, _anno, name, arity, clauses} when name != :__info__ <- abstract_code do
      clauses = AbstractTranslator.to_tria!(clauses, env: empty_env(module))
      kind = if {name, arity} in exports, do: :def, else: :defp
      {{module, kind, name, arity}, clauses}
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

  @doc """
  Fetches tria, but without top-level `fn`, only the list of bodies
  """
  @spec tria_bodies(abstract_code(), MFArity.mfarity()) :: [Tria.t()] | nil
  def tria_bodies(abstract_code, mfarity) do
    with {:fn, _, clauses} <- tria(abstract_code, mfarity) do
      Enum.map(clauses, fn {:"->", _, [_, body]} -> body end)
    end
  end

  @doc """
  Fetches abstract_code for mfa
  """
  @spec abstract(abstract_code(), atom(), arity()) :: abstract_code() | nil
  def abstract(abstract_code, name, arity) when is_atom(name) and is_integer(arity) and arity >= 0 do
    Enum.find_value(abstract_code, fn
      {:function, _anno, ^name, ^arity, clauses} -> clauses
      _ -> false
    end)
  end

  @doc """
  Returns a list of functions from abstract_code for passed `module`
  """
  @spec functions(abstract_code()) :: [{atom(), arity()}]
  def functions([{:function, _anno, name, arity, _clauses} | tail]) do
    [{name, arity} | functions(tail)]
  end
  def functions([_ | tail]) do
    functions(tail)
  end
  def functions([]), do: []

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

  @doc """
  Extracts docs from BEAM's object_code. Prints debug information
  """
  @spec docs(object_code()) :: {String.t() | nil, %{{atom(), arity()} => String.t()}}
  def docs(binary) do
    {:ok, doc} = chunk(binary, ~c"Docs")
    :erlang.binary_to_term(doc)
  rescue
    ArgumentError ->
      module = module(binary)
      Debug.puts "Non BERT docs format for #{module}, continuing without docs"
      {nil, %{}}

    MatchError ->
      module = module(binary)
      Debug.puts "Failed to fetch docs chunk for #{module}, continuing without docs"
      {nil, %{}}
  else
    {:docs_v1, _, :elixir, "text/markdown", maybe_moduledoc, _, docs} ->
      function_docs =
        for {{k, name, arity}, _, _, %{"en" => doc}, _} when k in ~w[macro function]a <- docs, into: %{} do
          {{name, arity}, doc}
        end

      moduledoc =
        case maybe_moduledoc do
          %{"en" => moduledoc} -> moduledoc
          _ -> nil
        end

      {moduledoc, function_docs}

    docs ->
      module = module(binary)
      Debug.puts "Unexpected docs format for #{module}, #{inspect docs}, continuing without docs"
      {nil, %{}}
  end

  @spec ensure_storage() :: :ets.tid()
  defp ensure_storage do
    with :undefined <- :ets.whereis(__MODULE__) do
      :ets.new(__MODULE__, [:set, :named_table, :public])
    end
  end

end
