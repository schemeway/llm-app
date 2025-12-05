defmodule LlmChat.Event do
  @moduledoc """
  Ecto schema for storing individual conversation events.
  Each event represents a message (user, assistant, thought, or tool).
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "events" do
    field(:role, :string)
    field(:content, :string)
    field(:model_id, :string)

    belongs_to(:conversation, LlmChat.Conversation, type: :binary_id)

    timestamps(updated_at: false)
  end

  @doc """
  Creates a changeset for an event.
  """
  def changeset(event, attrs) do
    event
    |> cast(attrs, [:conversation_id, :role, :content, :model_id])
    |> validate_required([:conversation_id, :role])
    |> foreign_key_constraint(:conversation_id)
  end
end
