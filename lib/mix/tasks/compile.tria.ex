defmodule Mix.Tasks.Compile.Tria do
  use Mix.Task.Compiler

  def run(_args) do
    if is_binary System.get_env "TRIA_DEBUG" do
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

    build_to = Path.join [
      Mix.Project.build_path(mix_config),
      "lib",
      to_string(mix_config[:project])
    ]

    File.mkdir_p!(build_to)

    elixirc_paths
    |> find_all_files()
    |> compile_files!(build_to)

    mix_config |> IO.inspect(limit: :infinity)

    :ok
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

  def quote_files(paths) do
    Enum.map(paths, fn file ->
      quoted =
        file
        |> File.read!()
        |> Code.string_to_quoted!()

      {file, quoted}
    end)
  end

  def compile_files!(files, build_to) do
    files
    |> quote_files()
    |> Enum.each(fn {file, quoted} ->
      quoted
      |> Tria.Compiler.compile(%{file: file})
      |> Enum.each(fn {module, binary} ->
        filename = Path.join(build_to, "#{module}.beam")
        IO.puts "#{module} -> #{filename}"
        File.write!(filename, binary)
      end)
    end)
  end

end
