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
          |> info()
          |> List.wrap()
        else
          []
        end
      end)

    confirmed? =
      with false <- args in [["-y"], ["--yes"]] do
        result = IO.gets("Confirm? [Y(yes); n(no)] ") in ["y\n", "Y\n", "\n"]
        unless result do
          IO.write [IO.ANSI.red(), "Denied. Nothing cleaned\n", IO.ANSI.reset()]
        end
        result
      end

    if confirmed? do
      Enum.each(to_delete, &File.rm/1)
    end
  end

  defp info(cache) do
    IO.write [
      "Deleting file: ",
      IO.ANSI.green(),
      cache,
      IO.ANSI.reset(),
      "\n"
    ]

    cache
  end

end
