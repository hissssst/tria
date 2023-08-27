defmodule Mix.Tasks.Tria.Report do
  use Mix.Task

  @moduledoc """
  # Reports Tria bug

  Simple task which opens up your default web browser
  with all automatically gatherable information prefilled
  in a github issue in compiler's tracker

  ### Example:

  Basically you may want to run something like this once

  ```sh
  $ TRIA_DEBUG=1 mix compile > file.log
  $ mix tria.report "Bug in compilation"
  ```
  """

  @shortdoc "Creates issue on compiler's tracker automatically"

  defmacrop safe(code) do
    quote do
      try do
        unquote(code)
      rescue
        _ -> nil
      end
    end
  end

  def run(args) do
    title = List.first(args) || "INSERT TITLE HERE"
    query = URI.encode_query(%{title: "Report: #{title}", body: body(title)})
    uri = "https://github.com/hissssst/tria/issues/new?" <> query
    System.cmd("xdg-open", [uri])
  end

  defp body(title) do
    """
    # #{title}

    <!-- Short desription of an issue and a link to the source code here -->

    ## Environment

    ### Versions

    * System info: `#{safe inspect :os.type()} #{safe inspect :os.version()}`
    * Erlang: `#{safe List.to_string :erlang.system_info :system_version}`
    * Elixir: `#{safe System.version()}`
    * Tria: `#{Tria.version()}`

    ### Configuration

    * compilers: `#{safe inspect Mix.Project.config()[:compilers]}`
    * protocol consolidation: `#{safe inspect Mix.Project.config()[:consolidate_protocols]}`
    * tria app env: `#{safe inspect Application.get_all_env(:tria)}`

    ### System environment

    ```
    #{safe system_env()}
    ```

    ## Expected behaiour

    <!-- Desription of how you think Tria should behave -->

    ## Actual behaviour

    <!-- Desription of how Tria actually behaved -->

    ## Steps to reproduce

    <!-- Steps to follow to reporduce the bug. The more precise the steps are, the better -->
    """
  end

  @vars ~w[
    TRIA_DEBUG
    TRIA_TRACE
  ]

  defp system_env do
    Enum.map_join(@vars, "\n", fn var ->
      "#{var}=#{System.get_env(var)}"
    end)
  end
end
