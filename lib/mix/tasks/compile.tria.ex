defmodule Mix.Tasks.Compile.Tria do

  use Mix.Task.Compiler

  alias Tria.Compiler
  alias Tria.Debug.Tracer

  def run(_args) do
    Mix.Project.get!() # Just to make sure that project exists
    mix_config = Mix.Project.config()
    Mix.Project.ensure_structure(mix_config)

    [
      # Write MFAs you want to trace here
    ]
    |> Enum.map(&Tracer.trace(&1, only: :all))

    root = Path.dirname Mix.Project.project_file()

    elixirc_paths =
      mix_config
      |> Keyword.fetch!(:elixirc_paths)
      |> Enum.map(fn path -> Path.join(root, path) end)

    build_path = Path.join [
      Mix.Project.build_path(mix_config),
      "lib",
      to_string(mix_config[:project] || mix_config[:app]),
      "ebin"
    ]

    File.mkdir_p!(build_path)

    compile(elixirc_paths, build_path)
  end

  # Basically the whole compilation pipeline
  defp compile(elixirc_paths, build_path) do
    compiled =
      elixirc_paths
      |> find_all_files()
      |> Compiler.compile(build_path: build_path, context: TriaGlobalContext)

    write_to_disk(compiled, build_path)
  end

  defp write_to_disk(modules, build_path) do
    Enum.each(modules, fn {module, binary} ->
      Compiler.save(build_path, module, binary)
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

        other ->
          raise "Unrecognized filetype #{inspect other} for #{path}"
      end
    end)
  end

end
