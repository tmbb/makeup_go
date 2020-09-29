defmodule MakeupGo.Mixfile do
  use Mix.Project

  def project do
    [
      app: :makeup_go,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      # Package
      package: package(),
      description: description(),
      docs: [
        main: "readme",
        extras: [
          "README.md"
        ]
      ]
    ]
  end

  defp description do
    """
    Go lexer for the Makeup syntax highlighter.
    """
  end

  defp package do
    [
      name: :makeup_go,
      licenses: ["BSD"],
      maintainers: ["Tiago Barroso <tmbb@campus.ul.pt>"],
      links: %{"GitHub" => "https://github.com/tmbb/makeup_go"}
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Makeup.Lexers.GoLexer.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:makeup, "~> 1.0"},
      {:nimble_parsec, "~> 1.0", override: true},
      {:unicode_set, "~> 0.8.0", only: [:dev]},
      {:benchee, "~> 1.0", only: [:dev]},
      {:ex_doc, "~> 0.22", only: [:dev]}
    ]
  end
end
