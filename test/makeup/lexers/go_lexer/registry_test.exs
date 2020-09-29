defmodule Makeup.Lexers.GoLexer.RegistryTest do
  use ExUnit.Case, async: true

  alias Makeup.Registry
  alias Makeup.Lexers.GoLexer

  describe "the go lexer has successfully registered itself:" do
    test "language name" do
      assert {:ok, {GoLexer, []}} == Registry.fetch_lexer_by_name("golang")
      assert {:ok, {GoLexer, []}} == Registry.fetch_lexer_by_name("go")
    end

    test "file extension" do
      assert {:ok, {GoLexer, []}} == Registry.fetch_lexer_by_extension("go")
    end
  end
end
