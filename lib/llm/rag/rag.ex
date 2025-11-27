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
    query_embedding = Client.embed_text(text)

    Repo.all(
      from(c in Chunk,
        order_by: cosine_distance(c.embedding, ^Pgvector.new(query_embedding)),
        limit: ^limit
      )
    )
  end
end
