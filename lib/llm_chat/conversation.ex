defmodule LlmChat.Conversation do
  @moduledoc """
  Ecto schema for storing conversations in PostgreSQL.
  Events are stored in a separate table with a has_many association.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "conversations" do
    field(:title, :string)

    has_many(:events, LlmChat.Event)

    timestamps()
  end

  @doc """
  Creates a changeset for a conversation.
  """
  def changeset(conversation, attrs) do
    conversation
    |> cast(attrs, [:id, :title])
    |> validate_required([:id])
  end
end
