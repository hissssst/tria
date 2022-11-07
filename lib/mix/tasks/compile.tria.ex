defmodule Mix.Tasks.Compile.Tria do
  use Mix.Task.Compiler

  alias Tria.Compiler

  def run(_args) do
    unless String.downcase(System.get_env("TRIA_DEBUG", "")) in ["", "0", "false", "no"] do
      :observer.start()
    end

    Mix.Project.get!() # Just to make sure that project exists
    mix_config = Mix.Project.config()
    Mix.Project.ensure_structure(mix_config)
    # manifest_path = Mix.Project.manifest_path(mix_config)

    root = Path.dirname Mix.Project.project_file()

    elixirc_paths =
      mix_config
      |> Keyword.fetch!(:elixirc_paths)
      |> Enum.map(fn path -> Path.join(root, path) end)

    build_path = Path.join [
      Mix.Project.build_path(mix_config),
      "lib",
      to_string(mix_config[:project])
    ]

    File.mkdir_p!(build_path)

    compile(elixirc_paths, build_path)
  end

  # Basically the whole compilation pipeline
  defp compile(elixirc_paths, build_path) do
    elixirc_paths
    |> find_all_files()
    |> Enum.map(fn step -> Compiler.to_quoted(step) end)
    |> Enum.map(fn step -> Compiler.start_compilation(step) end)
    |> Enum.map(fn step -> Compiler.finish_compilation(step) end)
    |> Enum.map(fn {modules, %{context: context}} ->
      write_to_disk(modules, build_path)
      context
    end)
    |> Enum.uniq()
    |> Enum.flat_map(fn context -> Compiler.generate_context(context) end)
    |> write_to_disk(build_path)
  end

  defp write_to_disk(pairs, build_path) do
    Enum.each(pairs, fn {module, binary} ->
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
