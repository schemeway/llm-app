defmodule LlmChat.Repo.Migrations.CreateEvents do
  use Ecto.Migration

  def change do
    create table(:events) do
      add(:conversation_id, references(:conversations, type: :uuid, on_delete: :delete_all),
        null: false
      )

      add(:role, :string, null: false)
      add(:content, :text)
      add(:model_id, :string)

      timestamps(updated_at: false)
    end

    create(index(:events, [:conversation_id]))

    alter table(:conversations) do
      remove(:events)
    end
  end
end
