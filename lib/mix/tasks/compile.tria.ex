defmodule Mix.Tasks.Compile.Tria do

  @moduledoc """
  Mix task for running Tria optimizing compiler
  """

  use Mix.Task.Compiler

  alias Mix.Project
  alias Tria.Compiler
  alias Tria.Compiler.Manifest
  alias Tria.Debug
  alias Tria.Debug.Tracer

  defmodule Options do
    # Internal representation `mix compile.tria` command
    @moduledoc false
    defstruct [force: false]
    def parse(["--force" | tail]), do: %__MODULE__{parse(tail) | force: true}
    def parse(_), do: %__MODULE__{}
  end

  def run(args) do
    options = Options.parse(args)
    Project.get!() # Just to make sure that project exists
    mix_config = Project.config()
    Project.ensure_structure(mix_config)
    manifest_path =
      mix_config
      |> Project.manifest_path()
      |> Path.join("tria.manifest")

    if truthy_string? System.get_env("TRIA_DEBUG", "false") do
      Debug.flag_debug()
    end

    if Debug.debugging?() do
      "TRIA_TRACE"
      |> System.get_env("")
      |> Tracer.parse_trace_env()
      |> Enum.each(&Tracer.trace(&1, only: :all))
    end

    root = Path.dirname Project.project_file()

    elixirc_paths =
      mix_config
      |> Keyword.fetch!(:elixirc_paths)
      |> Enum.map(fn path -> Path.join(root, path) end)

    build_path = Path.join [
      Project.build_path(mix_config),
      "lib",
      to_string(mix_config[:project] || mix_config[:app]),
      "ebin"
    ]

    File.mkdir_p!(build_path)

    compile(elixirc_paths, build_path, manifest_path, options)
  end

  defp compile(elixirc_paths, build_path, manifest_path, %Options{} = options) do
    manifest =
      if options.force do
        %Manifest{}
      else
        read_manifest(manifest_path)
      end

    {manifest, modules} =
      elixirc_paths
      |> find_all_files()
      |> Compiler.compile(manifest: manifest, build_path: build_path, context: TriaGlobalContext)

    save_manifest(manifest_path, manifest)
    write_to_disk(modules, build_path)
  end

  defp write_to_disk(modules, build_path) do
    Enum.each(modules, fn {module, binary} ->
      save(build_path, module, binary)
    end)
  end

  def find_all_files(paths) do
    Enum.flat_map(paths, fn path ->
      case File.stat!(path) do
        %{type: :directory} ->
          path
          |> File.ls!()
          |> Enum.map(&Path.join(path, &1))
          |> find_all_files()

        %{type: :regular} ->
          case Path.extname(path) do
            ".ex" -> [path]
            _ -> []
          end

        %{type: other} ->
          IO.warn "Unrecognized filetype #{inspect other} for #{path}"
          []
      end
    end)
  end

  defp truthy_string?(string) do
    import String

    string = trim downcase string
    string in ~w[true 1 yes y]
  end

  defp read_manifest(manifest_path) do
    case File.read(manifest_path) do
      {:ok, data} ->
        %Manifest{} = :erlang.binary_to_term(data)

      _ ->
        %Manifest{}
    end
  end

  defp save_manifest(manifest_path, %Manifest{} = manifest) do
    manifest_path
    |> Path.dirname()
    |> File.mkdir_p!()

    Debug.inspect(manifest, label: :manifest)

    File.write!(manifest_path, :erlang.term_to_binary(manifest))
  end

  defp save(build_path, module, binary) do
    filename = Path.join(build_path, "#{module}.beam")
    File.write!(filename, binary)

    [
      ignore_module_conflict: true
    ]
    |> Tria.Compiler.ElixirCompiler.with_compiler_options(fn ->
      :code.add_path to_charlist Path.dirname filename
      :code.purge module
      :code.load_file module
    end)
  end

end
