defmodule Mix.Tasks.Tria.Clean do
  use Mix.Task

  @moduledoc """
  # Cache cleanup

  Tria uses cache ets files located in `priv` directory to
  optimize evaluation of some function traits like purity
  or safety. This manually deletes the caches.

  ### Example:

  ```sh
  mix tria.clean -y
  ```

  ### Options

  * `-y` or `--yes` -- to skip confirmation step
  """

  @shortdoc "Deletes tria cache files"

  def run(args) do
    priv = "#{:code.priv_dir(:tria)}"

    to_delete =
      priv
      |> File.ls!()
      |> Enum.flat_map(fn filename ->
        if String.ends_with?(filename, "_cache.ets") do
          priv
          |> Path.join(filename)
          |> info("Cache file: ")
          |> List.wrap()
        else
          []
        end
      end)

    confirmed? =
      "-y" in args or
        "--yes" in args or
        IO.gets("Confirm? [Y(yes); n(no)] ") in ["y\n", "Y\n", "\n"]

    if confirmed? and to_delete != [] do
      Enum.each(to_delete, fn file ->
        info(file, "Deleting: ")
        File.rm!(file)
      end)
    else
      if to_delete == [] do
        IO.write([IO.ANSI.yellow(), "No cache files found!\n", IO.ANSI.reset()])
      end

      IO.write([IO.ANSI.red(), "Nothing cleaned\n", IO.ANSI.reset()])
    end
  end

  defp info(cache, prefix) do
    IO.write([
      prefix,
      IO.ANSI.green(),
      cache,
      IO.ANSI.reset(),
      "\n"
    ])

    cache
  end
end
