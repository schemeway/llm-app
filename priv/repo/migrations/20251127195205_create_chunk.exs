defmodule LlmChat.Repo.Migrations.CreateChunk do
  use Ecto.Migration

  def change do
    create table(:chunks) do
      add(:text, :string)
      add(:embedding, :vector)
    end
  end
end
