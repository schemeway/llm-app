defmodule Llm.Rag do
  require Logger

  alias Llm.Rag.Chunk
  alias Llm.PlatformRegistry
  alias LlmChat.Repo

  import Ecto.Query
  import Pgvector.Ecto.Query

  def store_text(text) do
    embedding = embed_text(text)

    %Chunk{
      text: text,
      embedding: embedding
    }
    |> Chunk.changeset(%{})
    |> Repo.insert()
  end

  def query_text(text, limit \\ 5) do
    query_embedding = embed_text(text) |> Pgvector.new()

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
        embedding: embed_text(chunk.text)
      }
      |> Chunk.changeset(%{})
      |> Repo.insert()
    end)
  end

  def clean() do
    Repo.delete_all(Chunk)
  end

  def count_documents() do
    Repo.aggregate(Chunk, :count, :id)
  end

  def ingest_document(file_path) do
    Logger.debug("Ingesting document: #{file_path}")

    with {:ok, content} <- File.read(file_path),
         {:ok, text} <- Tools.WebScraper.extract_text_content(content) do
      chunk_text(text)
    end
  end

  def ingest_documents(file_paths) do
    Enum.map(file_paths, &ingest_document/1)
  end

  defp embed_text(text) do
    PlatformRegistry.default_platform().embed_text(text)
  end
end
