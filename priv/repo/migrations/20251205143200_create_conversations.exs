defmodule LlmChat.Repo.Migrations.CreateConversations do
  use Ecto.Migration

  def change do
    create table(:conversations, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:title, :string)
      add(:events, :text)

      timestamps()
    end

    create(index(:conversations, [:inserted_at]))
  end
end
