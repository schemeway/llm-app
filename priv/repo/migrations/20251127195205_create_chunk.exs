defmodule LlmChat.Repo.Migrations.CreateChunk do
  use Ecto.Migration

  def change do
    create table(:chunks) do
      add(:text, :text)
      add(:embedding, :vector)
    end
  end
end
