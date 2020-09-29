defmodule StringVsCharBenchmark do
  import NimbleParsec

  hex_digit_chars = [?0..?9, ?a..?f, ?A..?F]
  hex_digit = ascii_char(hex_digit_chars)

  big_u_chars = string("\\U") |> times(hex_digit, 8)
  big_u_string = string("\\U") |> ascii_string(hex_digit_chars, 8)

  defparsec(:big_u_chars, big_u_chars)
  defparsec(:big_u_string, big_u_string)

  def run() do
    text = "\\U16fe7ff"

    Benchee.run(%{
      "ascii_char" => fn -> big_u_chars(text) end,
      "ascii_string" => fn -> big_u_string(text) end
    })
  end
end

StringVsCharBenchmark.run()