defmodule Tria.MixProject do
  use Mix.Project

  def project do
    [
      app: :tria,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.23.0", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0.0", only: :dev, runtime: false},
      {:credo, "~> 1.1", only: :dev, runtime: false},

      {:pathex, path: "../../pathex"},
    ]
  end
end
