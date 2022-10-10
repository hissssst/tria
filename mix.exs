defmodule Tria.MixProject do
  use Mix.Project

  def project do
    [
      app: :tria,
      version: "0.0.1",
      elixir: "~> 1.11",
      start_permanent: false,
      elixirc_paths: elixirc_paths(),
      deps: deps()
    ]
  end

  def application do
    []
  end

  defp elixirc_paths(env \\ Mix.env)
  defp elixirc_paths(:test), do: elixirc_paths(:prod) ++ ["test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:ex_doc,   "~> 0.23.0", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0.0",  only: :dev, runtime: false},
      {:credo,    "~> 1.1",    only: :dev, runtime: false},
    ]
  end
end
