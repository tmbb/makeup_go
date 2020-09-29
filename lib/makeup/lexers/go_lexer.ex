defmodule Makeup.Lexers.GoLexer do
  @moduledoc """
  Lexer for the Go language to be used
  with the Makeup package.
  """
  @behaviour Makeup.Lexer

  import NimbleParsec
  import Makeup.Lexer.Combinators
  import Makeup.Lexer.Groups
  alias Makeup.Lexers.GoLexer.Utf8Utils

  ###################################################################
  # Step #1: tokenize the input (into a list of tokens)
  ###################################################################

  whitespace = ascii_string([?\s, ?\t, ?\n], min: 1) |> token(:whitespace)

  # This is the combinator that ensures that the lexer will never reject a file
  # because of invalid input syntax
  any_char = utf8_char([]) |> token(:error)

  # From the spec:
  #
  #    newline        = /* the Unicode code point U+000A */ .
  #    unicode_char   = /* an arbitrary Unicode code point except newline */ .
  #    unicode_letter = /* a Unicode code point classified as "Letter" */ .
  #    unicode_digit  = /* a Unicode code point classified as "Number, decimal digit" */ .

  _newline = ascii_char([?\n])
  unicode_char = utf8_char([not: ?\n])
  _unicode_letter = utf8_char(Utf8Utils.unicode_letters())
  _unicode_digit = utf8_char(Utf8Utils.unicode_digits())

  # From the spec:
  #
  #    letter        = unicode_letter | "_" .
  #    decimal_digit = "0" … "9" .
  #    binary_digit  = "0" | "1" .
  #    octal_digit   = "0" … "7" .
  #    hex_digit     = "0" … "9" | "A" … "F" | "a" … "f" .

  letter = utf8_char([?_] ++ Utf8Utils.unicode_letters())
  # Not in the spec but probably more efficient than choices([letter, unicode_digit])
  letter_or_unicode_digit =
    utf8_char(
      [?_] ++
        Utf8Utils.unicode_letters() ++
        Utf8Utils.unicode_digits()
    )

  decimal_digit = ascii_char([?0..?9])
  binary_digit = ascii_char([?0..?9])
  octal_digit = ascii_char([?0..?9])
  hex_digit = ascii_char([?0..?9, ?a..?f, ?A..?F])


  # Identifiers. From the spec:
  #
  #     identifier = letter { letter | unicode_digit } .

  identifier = letter |> repeat(letter_or_unicode_digit)

  # We need identifiers to be binaries so that we can postprocess them later
  identifier_token =
    identifier
    |> lexeme()
    |> token(:name)

  # From the spec:
  #
  #     decimal_digits = decimal_digit { [ "_" ] decimal_digit } .
  #     binary_digits  = binary_digit { [ "_" ] binary_digit } .
  #     octal_digits   = octal_digit { [ "_" ] octal_digit } .
  #     hex_digits     = hex_digit { [ "_" ] hex_digit } .

  make_digits = fn combinator ->
    combinator |> repeat(optional(string("_")) |> concat(combinator))
  end

  decimal_digits = make_digits.(decimal_digit)
  binary_digits = make_digits.(binary_digit)
  octal_digits = make_digits.(octal_digit)
  hex_digits = make_digits.(hex_digit)

  # From the spec:

  #     int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
  #     decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
  #     binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
  #     octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
  #     hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .

  make_int_lit = fn markers, digits ->
    string("0")
    |> ascii_char(markers)
    |> optional(string("_"))
    |> concat(digits)
  end

  decimal_lit =
    choice([
      string("0"),
      ascii_char([?0..?9])
      |> optional(string("_"))
      |> concat(decimal_digits)
    ])

  decimal_lit_token = token(decimal_lit, :number_integer)

  binary_lit = make_int_lit.([?b, ?B], binary_digits)
  binary_lit_token = token(binary_lit, :number_bin)

  octal_lit = make_int_lit.([?b, ?B], octal_digits)
  octal_lit_token = token(octal_lit, :number_oct)

  hex_lit = make_int_lit.([?b, ?B], hex_digits)
  hex_lit_token = token(hex_lit, :number_hex)

  int_lit =
    choice([
      decimal_lit,
      binary_lit,
      octal_lit,
      hex_lit
    ])

  int_lit_token =
    choice([
      decimal_lit_token,
      binary_lit_token,
      octal_lit_token,
      hex_lit_token
    ])

  # From the spec:
  #
  #     float_lit         = decimal_float_lit | hex_float_lit .
  #
  #     decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
  #                         decimal_digits decimal_exponent |
  #                         "." decimal_digits [ decimal_exponent ] .
  #     decimal_exponent  = ( "e" | "E" ) [ "+" | "-" ] decimal_digits .
  #
  #     hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
  #     hex_mantissa      = [ "_" ] hex_digits "." [ hex_digits ] |
  #                         [ "_" ] hex_digits |
  #                         "." hex_digits .
  #     hex_exponent      = ( "p" | "P" ) [ "+" | "-" ] decimal_digits .

  decimal_exponent =
    ascii_char([?e, ?E])
    |> ascii_char([?+, ?-])
    |> concat(decimal_digits)

  decimal_float_lit =
    choice([
      # First format
      decimal_digits
      |> string(".")
      |> optional(decimal_digits)
      |> optional(decimal_exponent),
      # Second format
      decimal_digits
      |> concat(decimal_exponent)
    ])

  hex_exponent =
    ascii_char([?p, ?P])
    |> ascii_char([?+, ?-])
    |> concat(decimal_digits)

  hex_mantissa =
    choice([
      # First format
      optional(string("_"))
      |> concat(hex_digits)
      |> string(".")
      |> optional(hex_digits),
      # Second format
      optional(string("_"))
      |> concat(hex_digits),
      # Third format
      string(".")
      |> concat(hex_digits)
    ])

  hex_float_lit =
    string("0")
    |> ascii_char([?x, ?X])
    |> concat(hex_mantissa)
    |> concat(hex_exponent)

  float_lit =
    choice([
      decimal_float_lit,
      hex_float_lit
    ])

  float_lit_token = token(float_lit, :number_float)

  # From the spec:
  #
  #     imaginary_lit = (decimal_digits | int_lit | float_lit) "i" .

  imaginary_lit =
    choice([
      decimal_digits,
      int_lit,
      float_lit
    ])
    |> string("i")

  imaginary_lit_token = token(imaginary_lit, :number_integer)

  # Runes. From the spec:
  #
  #     rune_lit         = "'" ( unicode_value | byte_value ) "'" .
  #     unicode_value    = unicode_char | little_u_value | big_u_value | escaped_char .
  #     byte_value       = octal_byte_value | hex_byte_value .
  #     octal_byte_value = `\` octal_digit octal_digit octal_digit .
  #     hex_byte_value   = `\` "x" hex_digit hex_digit .
  #     little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
  #     big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
  #                               hex_digit hex_digit hex_digit hex_digit .
  #     escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .

  escaped_char =
    string("\\")
    |> ascii_char([?a, ?b, ?f, ?n, ?r, ?t, ?v, ?\\, ?', ?"])

  big_u_value = string("\\U") |> times(hex_digit, 8)
  little_u_value = string("\\u") |> times(hex_digit, 4)

  hex_byte_value = string("\\x") |> times(hex_digit, 2)
  octal_byte_value = string("\\") |> times(octal_digit, 3)

  byte_value =
    choice([
      octal_byte_value,
      hex_byte_value
    ])

  unicode_value =
    choice([
      unicode_char,
      little_u_value,
      big_u_value,
      escaped_char
    ])

  rune_lit =
    string("'")
    |> choice([unicode_value, byte_value])
    |> string("'")

  rune_lit_token = token(rune_lit, :string_char)

  # Strings. From the spec:
  #
  #    string_lit             = raw_string_lit | interpreted_string_lit .
  #    raw_string_lit         = "`" { unicode_char | newline } "`" .
  #    interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
  #
  # Here we deviate from the ABNF spec due to efficiency reasons.
  # Using the pre-made Makeup combinators leads to better backtracking

  raw_string_lit_token =
    string("`")
    |> repeat(ascii_char([not: ?`]))
    |> string("`")
    |> token(:string_backtick)

  special_char_in_string =
    choice([
      little_u_value,
      big_u_value,
      escaped_char,
      byte_value
    ])
    |> token(:string_escape)

  # This implementation is slightly incorrect because it allows newlines inside strings
  # TODO: fix it later
  interpreted_string_lit_token =
    string_like(
      string("\""),
      string("\""),
      [special_char_in_string],
      :string
    )

  # Operators
  operators = ~w[
    <<= >>= << >> <= >= &^= &^ += -= *= /= %=
    &= |= && || <- ++ -- == != := ... + - * / % &
  ]

  operator_token = word_from_list(operators) |> token(:operator)

  punctuation = ~w[| ^ < > = | ( ) \[ \] { } . , ; :]

  punctuation_token = word_from_list(punctuation) |> token(:punctuation)

  # Tag the tokens with the language name.
  # This makes it easier to postprocess files with multiple languages.
  @doc false
  def __as_go_language__({ttype, meta, value}) do
    {ttype, Map.put(meta, :language, :go), value}
  end

  root_element_combinator =
    choice([
      whitespace,
      # Identifiers (= variables)
      identifier_token,
      # Strings and Characters
      rune_lit_token,
      raw_string_lit_token,
      interpreted_string_lit_token,
      # Numbers
      imaginary_lit_token,
      float_lit_token,
      int_lit_token,
      # Operators
      operator_token,
      # Punctuation
      punctuation_token,
      # If we can't parse any of the above, we highlight the next character as an error
      # and proceed from there.
      # A lexer should always consume any string given as input.
      any_char
    ])

  ##############################################################################
  # Semi-public API: these two functions can be used by someone who wants to
  # embed this lexer into another lexer, but other than that, they are not
  # meant to be used by end-users
  ##############################################################################

  @inline Application.get_env(:makeup_go, :inline, false)

  @impl Makeup.Lexer
  defparsec(
    :root_element,
    root_element_combinator |> map({__MODULE__, :__as_go_language__, []}),
    inline: @inline
  )

  @impl Makeup.Lexer
  defparsec(
    :root,
    repeat(parsec(:root_element)),
    inline: @inline
  )

  ###################################################################
  # Step #2: postprocess the list of tokens
  ###################################################################

  @keyword ~w[
    break default select case defer go else goto
    switch fallthrough if range continue for return
  ]

  @keyword_declaration ~w[var func strut map chan type interface const]

  @keyword_namespace ~w[import package]

  @keyword_constant ~w[true false iota nil]

  @name_builtin ~w[
    uint uint8 uint16 uint32 uint64
    int int8 int16 int 32 int64
    float float32 float64
    complex63 complex128 byte rune
    string bool error uintptr
    print println panic recover close complex
    real imag len cap append copy delete
  ]

  @keyword_type ~w[
    uint uint8 uint16 uint32 unit64
    int int8 int16 int32 int64
    float float32 float64
    complex64 complex 128 byte rune
    string bool error uintptr
  ]

  @impl Makeup.Lexer
  def postprocess(tokens, opts \\ [])

  def postprocess([{name, meta, :variable} | tokens], opts)
      when name in @keyword do
    [{name, meta, :keyword} | postprocess(tokens, opts)]
  end

  def postprocess([{name, meta, :variable} | tokens], opts)
      when name in @keyword_declaration do
    [{name, meta, :keyword_declaration} | postprocess(tokens, opts)]
  end

  def postprocess([{name, meta, :variable} | tokens], opts)
      when name in @keyword_namespace do
    [{name, meta, :keyword_namespace} | postprocess(tokens, opts)]
  end

  def postprocess([{name, meta, :variable} | tokens], opts)
      when name in @keyword_constant do
    [{name, meta, :keyword_constant} | postprocess(tokens, opts)]
  end

  def postprocess([{name, meta, :variable}, paren = {"(", _, :punctuation} | tokens], opts)
      when name in @name_builtin do
    [{name, meta, :name_builtin}, paren | postprocess(tokens, opts)]
  end

  def postprocess([{name, meta, :variable} | tokens], opts)
      when name in @keyword_type do
    [{name, meta, :name_builtin} | postprocess(tokens, opts)]
  end

  def postprocess([token | tokens], opts) do
    [token | postprocess(tokens, opts)]
  end

  def postprocess([], _opts) do
    []
  end

  #######################################################################
  # Step #3: highlight matching delimiters
  # By default, this includes delimiters that are used in many languages,
  # but feel free to delete these or add more.
  #######################################################################

  @impl Makeup.Lexer
  defgroupmatcher(:match_groups,
    parentheses: [
      open: [
        [{:punctuation, _, "("}]
      ],
      close: [
        [{:punctuation, _, ")"}]
      ]
    ],
    straight_brackets: [
      open: [
        [{:punctuation, _, "["}]
      ],
      close: [
        [{:punctuation, _, "]"}]
      ]
    ],
    curly_braces: [
      open: [
        [{:punctuation, _, "{"}]
      ],
      close: [
        [{:punctuation, _, "}"}]
      ]
    ]
  )

  # Finally, the public API for the lexer
  @impl Makeup.Lexer
  def lex(text, opts \\ []) do
    group_prefix = Keyword.get(opts, :group_prefix, random_prefix(10))
    {:ok, tokens, "", _, _, _} = root(text)

    tokens
    |> postprocess()
    |> match_groups(group_prefix)
  end
end
