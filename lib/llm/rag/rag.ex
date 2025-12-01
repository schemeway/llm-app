defmodule Llm.Rag do
  alias Llm.Rag.Chunk
  alias Llm.Bedrock.Client
  alias LlmChat.Repo

  import Ecto.Query
  import Pgvector.Ecto.Query

  def store_text(text) do
    embedding = Client.embed_text(text)

    %Chunk{
      text: text,
      embedding: embedding
    }
    |> Chunk.changeset(%{})
    |> Repo.insert()
  end

  def query_text(text, limit \\ 5) do
    query_embedding = Client.embed_text(text) |> Pgvector.new()

    Repo.all(
      from(chunk in Chunk,
        order_by: cosine_distance(chunk.embedding, ^query_embedding),
        limit: ^limit
      )
    )
  end

  def chunk_text(text, format \\ :plaintext) do
    TextChunker.split(text, format: format)
    |> Enum.map(fn chunk ->
      %Chunk{
        text: chunk.text,
        embedding: Client.embed_text(chunk.text)
      }
      |> Chunk.changeset(%{})
      |> Repo.insert()
    end)
  end
end
