defmodule Tools.CurrencyRate do
  use Tools.Tool,
    description: """
    This tool provides the current exchange rate the given currency and "CAD".
    The input should be a string in the format "USD" or "EUR".
    """,
    parameters: %{
      from_currency: %{
        type: :string,
        description: "The currency you want to convert from."
      }
    }

  def call(%{"from_currency" => from_currency}) do
    exchange_rate = get_exchange_rate(from_currency)
    %{"result" => exchange_rate}
  end

  def get_exchange_rate(from_currency) do
    Req.get!(
      "https://bcd-api-dca-ipa.cbsa-asfc.cloud-nuage.canada.ca/exchange-rate-lambda/exchange-rates"
    )
    |> Map.get(:body)
    |> Map.get("ForeignExchangeRates")
    |> Enum.find(fn rate -> rate["FromCurrency"]["Value"] == from_currency end)
    |> Map.get("Rate")
    |> convert_to_float()
  end

  defp convert_to_float(string) do
    {value, _} = Float.parse(string)
    value
  end
end
