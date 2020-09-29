# Generate a file with the right character sequences
# so that `unicode_set` is not a dependency and is only
# required when one is developping `makeup_go`.
# The `unicode_set` library takes several minutes to
# compile, and those compile times are unacceptable
# if one only needs a bit of data that can be statically
# baked into a normal elixir file

indented =
  fn value, level ->
    text =
      value
      |> Macro.to_string()
      |> Code.format_string!()
      |> IO.iodata_to_binary()

    lines = String.split(text, "\n")
    spaces = String.duplicate(" ", level)
    indented_lines = Enum.map(lines, fn line -> [spaces, line, "\n"] end)
    IO.iodata_to_binary(indented_lines)
  end

# Why do we need to flatten these lists? A bug in `unicode_set`?
unicode_letters = Unicode.Set.to_utf8_char("[[:Letter:]]") |> List.flatten()
unicode_digits = Unicode.Set.to_utf8_char("[[:Nd:]]") |> List.flatten()

indented_unicode_letters = indented.(unicode_letters, 4)
indented_unicode_digits = indented.(unicode_digits, 4)

contents = """
defmodule Makeup.Lexers.GoLexer.Utf8Utils do
  @moduledoc false

  # This module is generated at "dev time" so that the lexer
  # doesn't have to depend on the (excelent) `unicode_set` library,
  # which takes several minutes to compile.
  #
  # The `unicode_se` library thus becomes a `:dev` dependency of `makeup_go`
  # and is required only during development.

  @doc \"""
  All unicode letters (`[[:Letter:]]`).

  A list of character ranges compatible with NimbleParsec
  \"""
  def unicode_letters() do
#{indented_unicode_letters}  end

  @doc \"""
  All unicode digits (`[[:Nd:]]`).

  A list of character ranges compatible with NimbleParsec
  \"""
  def unicode_digits() do
#{indented_unicode_digits}  end
end
"""

filename = "lib/makeup/lexers/go_lexer/utf8_utils.ex"

File.write!(filename, contents)