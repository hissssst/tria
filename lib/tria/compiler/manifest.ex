defmodule Tria.Compiler.Manifest do
  @moduledoc """
  Compiler manifest.

  This module provides state management for incremental compilation.
  """

  # Recompile when any input file changes
  # Recompile when any dependency of optimizer changes
  #   When dependency is internal, recompile it too

  alias Tria.Language.FunctionRepo
  alias Tria.Language.FunctionGraph
  alias Tria.Language.Meta
  alias Tria.Language.MFArity

  import :erlang, only: [term_to_binary: 1, md5: 1]

  @tria_version Tria.MixProject.project[:version]

  defstruct [
    new: true,
    tria_version: @tria_version,
    file_inputs: %{},
    file_to_modules: %{},
    function_inputs: %{},
    graphs: %{}
  ]

  @opaque hash :: binary()

  @type path :: Path.t()

  @type file_to_modules :: %{path() => MapSet.t(module())}

  @type diff(item) :: [{:added | :removed | :changed, item}]

  @typedoc """
  This structure represents state of inputs upon which the compilation occured
  """
  @type t :: %__MODULE__{
    file_inputs: %{path() => hash()},
    file_to_modules: file_to_modules(),
    function_inputs: %{MFArity.mfarity() => hash()},
    graphs: %{atom() => %{MFArity.t() => [MFArity.t()]}}
  }

  @doc """
  Takes all currently present (or passed) graphs and stores them in the manifest
  """
  @spec reflect_graphs(t(), [FunctionGraph.t()]) :: t()
  def reflect_graphs(
    %__MODULE__{graphs: existing_graphs} = manifest,
    graphs \\ FunctionGraph.all_graphs()
  ) do
    new_graphs = Map.new(graphs, fn graph -> {graph, FunctionGraph.to_map(graph)} end)
    graphs = Map.merge(existing_graphs, new_graphs, fn _k, l, r -> Map.merge(l, r) end)
    %__MODULE__{manifest | graphs: graphs}
  end

  @doc """
  Creates a manifest from files_to_modules relations and a list of
  mfarities. These mfarities are hashed from Beam and modules,
  and files are hashed too
  """
  @spec format(file_to_modules(), [MFArity.mfarity()]) :: t()
  def format(file_to_modules, functions) do
    file_hashes = hash_files Map.keys file_to_modules
    function_hashes = hash_functions functions

    %__MODULE__{
      file_inputs: file_hashes,
      file_to_modules: file_to_modules,
      function_inputs: function_hashes
    }
  end

  @spec diff_files(t(), [path()]) :: diff(path())
  def diff_files(%__MODULE__{file_inputs: was}, became) do
    became = hash_files became
    different_keys(was, became)
  end

  @spec infer_modules(t(), [path()]) :: MapSet.t(module())
  def infer_modules(%__MODULE__{file_to_modules: file_to_modules}, files) do
    Enum.reduce(files, MapSet.new(), fn file, acc ->
      case file_to_modules do
        %{^file => modules} ->
          MapSet.union(acc, modules)

        _ ->
          acc
      end
    end)
  end

  @spec infer_files(t(), Enumerable.t()) :: [path()]
  def infer_files(%__MODULE__{file_to_modules: file_to_modules}, modules) do
    Enum.flat_map(modules, fn module ->
      case Enum.find(file_to_modules, fn {_file, modules} -> module in modules end) do
        {file, _} -> [file]
        _ -> []
      end
    end)
  end

  @spec apply_diff(t(), diff(path())) :: t()
  def apply_diff(%__MODULE__{file_inputs: file_inputs} = manifest, diff) do
    file_inputs =
      Enum.reduce(diff, file_inputs, fn
        {:removed, file}, acc ->
          Map.delete(acc, file)

        {_, file}, acc ->
          Map.put(acc, file, hash_file(file))
      end)

    %__MODULE__{manifest | file_inputs: file_inputs}
  end

  @spec update_file_to_modules(t(), file_to_modules()) :: t()
  def update_file_to_modules(%__MODULE__{
    file_to_modules: file_to_modules,
    file_inputs: file_inputs
  } = manifest, new_file_to_modules \\ %{}) do
    file_to_modules =
      file_to_modules
      |> Map.merge(new_file_to_modules)
      |> Enum.reduce(%{}, fn {file, modules}, acc ->
        case file_inputs do
          %{^file => _} -> Map.put(acc, file, modules)
          _ -> acc
        end
      end)

    %__MODULE__{manifest | file_to_modules: file_to_modules}
  end

  #TODO optimize, load graph into ets and query ets
  #TODO `depends` relation is actually transitive, we can save time
  #by rerunning this function here, instead of relying on `each_cycle`
  @spec compile_time_dependants(t(), [module()]) :: MapSet.t(module())
  def compile_time_dependants(%__MODULE__{graphs: %{depends: graph}}, modules) do
    Enum.reduce(modules, MapSet.new(), fn module, acc ->
      new =
        Enum.reduce(graph, MapSet.new(), fn {{dependant, _, _}, depends_on}, acc ->
          if Enum.find(depends_on, &match?({^module, _, _}, &1)) do
            MapSet.put(acc, dependant)
          else
            acc
          end
        end)

      MapSet.union(acc, new)
    end)
  end
  def compile_time_dependants(%__MODULE__{graphs: %{}}, _modules), do: MapSet.new()

  ### Hashing

  @spec hash_file(path()) :: hash()
  def hash_file(file) do
    md5 File.read! file
  end

  @spec hash_files([path()]) :: %{path() => hash()}
  def hash_files(files) do
    Map.new(files, fn file -> {file, hash_file(file)} end)
  end

  @spec hash_function(MFArity.mfarity()) :: binary()
  def hash_function(mfarity) do
    hash_ast FunctionRepo.lookup(mfarity, :tria)
  end

  @spec hash_functions([MFArity.mfarity()]) :: %{MFArity.mfarity() => hash()}
  def hash_functions(mfarities) do
    Map.new(mfarities, fn mfarity -> {mfarity, hash_function(mfarity)} end)
  end

  @spec hash_ast(Tria.t()) :: binary()
  def hash_ast(nil), do: <<0>>
  def hash_ast(ast) do
    ast
    |> Meta.unmeta()
    |> term_to_binary()
    |> md5()
  end

  import :maps, only: [next: 1, iterator: 1, take: 2]

  @spec different_keys(map(), map()) :: diff(any())
  defp different_keys(left, right) do
    left
    |> iterator()
    |> next()
    |> do_diff(right)
  end

  defp do_diff(:none, right) when right == %{}, do: []
  defp do_diff(:none, right) do
    right
    |> Map.keys()
    |> Enum.map(fn key -> {:added, key} end)
  end
  defp do_diff({key, _, left}, right) when right == %{} do
    [{:removed, key} | do_diff(next(left), right)]
  end
  defp do_diff({key, value, left}, right) do
    left = next left
    case take(key, right) do
      {^value, right} -> do_diff(left, right)
      {_other, right} -> [{:changed, key} | do_diff(left, right)]
      :error -> [{:removed, key} | do_diff(left, right)]
    end
  end

end
