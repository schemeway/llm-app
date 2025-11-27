defmodule Llm.Rag.Chunk do
  use Ecto.Schema

  import Ecto.Changeset

  schema "chunks" do
    field(:text, :string)
    field(:embedding, Pgvector.Ecto.Vector)
  end

  def changeset(chunk, attrs) do
    chunk
    |> cast(attrs, [:text, :embedding])
    |> validate_required([:text, :embedding])
  end
end
