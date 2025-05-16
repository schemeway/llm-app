defmodule Tools.RomanConverter do
  require Logger

  use Tools.Tool,
    description: "A tool for converting Arabic numerals to Roman numerals.",
    parameters: %{
      "arabic_numeral" => %{
        "type" => "integer",
        "description" => "the Arabic numeral to convert"
      }
    }

  def call(%{"arabic_numeral" => arabic_numeral}) do
    Logger.debug("Converting #{arabic_numeral} to Roman numerals")
    result = arabic_to_roman(arabic_numeral)

    %{"result" => result}
  end

  # This function converts an Arabic numeral to a Roman numeral.
  def arabic_to_roman(arabic_numeral) do
    [
      {1000, "M"},
      {900, "CM"},
      {500, "D"},
      {400, "CD"},
      {100, "C"},
      {90, "XC"},
      {50, "L"},
      {40, "XL"},
      {10, "X"},
      {9, "IX"},
      {5, "V"},
      {4, "IV"},
      {1, "I"}
    ]
    |> Enum.reduce({arabic_numeral, ""}, fn {arabic, roman}, {remaining, result} ->
      if remaining >= arabic do
        duplicates = div(remaining, arabic)
        {remaining - arabic * duplicates, result <> String.duplicate(roman, duplicates)}
      else
        {remaining, result}
      end
    end)
    |> elem(1)
  end
end
