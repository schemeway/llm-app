defmodule Tools.Rag do
  use Tools.Tool,
    description: "A tool for retrieving relevant information from knowledge bases.",
    parameters: %{
      "query" => %{
        "type" => "string",
        "description" => "The query to search for in the knowledge base"
      }
    }

  def call(%{"query" => query}) do
    chunks =
      Llm.Rag.query_text(query)
      |> Enum.map(fn chunk -> chunk.text end)

    %{"result" => chunks}
  end
end
