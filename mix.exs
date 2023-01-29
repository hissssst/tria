defmodule Tria.MixProject do
  use Mix.Project

  def project do
    [
      app: :tria,
      version: "0.0.1",
      elixir: "~> 1.13",
      start_permanent: false,
      elixirc_paths: elixirc_paths(),
      erlc_options: [debug_info: true],
      test_elixirc_options: [debug_info: true, docs: true],
      package: package(),
      deps: deps()
    ]
  end

  def application, do: []

  def description do
    "Optimizing compiler for Elixir language"
  end

  defp package do
    [
      description: description(),
      licenses: ["GPL 3"],
      files: [
        "lib",
        "mix.exs",
        "README.md",
        "priv/pure.ets",
        ".formatter.exs"
      ],
      maintainers: [
        "Georgy Sychev"
      ],
      links: %{
        GitHub: "https://github.com/hissssst/tria"
      }
    ]
  end

  defp elixirc_paths(env \\ Mix.env)
  defp elixirc_paths(:test), do: elixirc_paths(:prod) ++ ["test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:ex_doc,   "~> 0.23.0", only: :docs, runtime: false},
      {:dialyxir, "~> 1.0.0",  only: :dev, runtime: false},
      {:credo,    "~> 1.1",    only: :dev, runtime: false},
      {:rexbug,   "~> 1.0",    only: :dev, runtime: false},
    ]
  end
end
