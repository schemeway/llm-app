defmodule Tools.WebScraper do
  require Logger

  use Tools.Tool,
    description:
      "A tool for scraping web content from a given URL. Returns the HTML content and can optionally extract text content.",
    parameters: %{
      "url" => %{
        "type" => "string",
        "description" => "the URL to scrape"
      },
      "extract_text" => %{
        "type" => "boolean",
        "description" => "whether to extract plain text content (default: false)"
      }
    }

  def call(%{"url" => url} = params) do
    extract_text = Map.get(params, "extract_text", false)
    Logger.debug("Scraping URL: #{url}")

    case make_request(url) do
      {:ok, html_content} ->
        if extract_text do
          case extract_text_content(html_content) do
            {:ok, text_content} ->
              %{
                "url" => url,
                "html_content" => html_content,
                "text_content" => text_content,
                "status" => "success"
              }

            {:error, reason} ->
              Logger.error("Failed to extract text from HTML: #{reason}")

              %{
                "url" => url,
                "html_content" => html_content,
                "error" => "Failed to extract text: #{reason}",
                "status" => "partial_success"
              }
          end
        else
          %{
            "url" => url,
            "html_content" => html_content,
            "status" => "success"
          }
        end

      {:error, reason} ->
        Logger.error("Failed to scrape URL #{url}: #{reason}")

        %{
          "url" => url,
          "error" => "Failed to scrape URL: #{reason}",
          "status" => "error"
        }
    end
  end

  defp make_request(url) do
    case Req.get(url,
           headers: [
             {"User-Agent", "Mozilla/5.0 (compatible; ElixirWebScraper/1.0)"}
           ],
           max_redirects: 5,
           receive_timeout: 30_000
         ) do
      {:ok, %{status: 200, body: body}} ->
        {:ok, body}

      {:ok, %{status: status}} ->
        {:error, "HTTP #{status}"}

      {:error, reason} ->
        {:error, inspect(reason)}
    end
  end

  def extract_text_content(html) do
    try do
      {:ok, document} = Floki.parse_document(html)

      text =
        document
        |> Floki.find("body")
        |> Floki.filter_out("script")
        |> Floki.filter_out("style")
        |> Floki.filter_out("button")
        |> Floki.filter_out("a")
        |> Floki.filter_out("svg")
        |> Floki.text(sep: " ")
        |> String.replace(~r/\s+/, " ")
        |> String.trim()

      {:ok, text}
    rescue
      error -> {:error, inspect(error)}
    end
  end
end
