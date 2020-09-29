defmodule Makeup.Lexers.GoLexer.Application do
  @moduledoc false
  use Application

  alias Makeup.Registry
  alias Makeup.Lexers.GoLexer

  def start(_type, _args) do
    Registry.register_lexer(GoLexer,
      options: [],
      names: ["golang", "go"],
      extensions: ["go"]
    )

    Supervisor.start_link([], strategy: :one_for_one)
  end
end
