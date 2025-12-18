defmodule Llm.History do
  @moduledoc """
  Manages conversation history using PostgreSQL storage.
  Events are stored in a dedicated table with creation time and model ID.
  """

  require Logger
  import Ecto.Query

  alias LlmChat.Repo
  alias LlmChat.Conversation
  alias LlmChat.Event

  @doc """
  Saves a conversation and its new event to the database.
  Returns updated history map with conversation id => title.
  """
  def update(history, id) do
    # Ensure conversation exists
    case Repo.get(Conversation, id) do
      nil -> history
      existing -> Map.put(history, id, existing.title)
    end
  end

  def save_event(conversation_id, message) do
    if Repo.get(Conversation, conversation_id) == nil do
      title = get_first_message([message]) |> fit_to_sql_string()

      %Conversation{}
      |> Conversation.changeset(%{id: conversation_id, title: title})
      |> Repo.insert!()
    end

    %Event{}
    |> Event.changeset(%{
      conversation_id: conversation_id,
      role: message["role"],
      content: encode_content(message["content"]),
      model_id: message["model_id"]
    })
    |> Repo.insert!()
  end

  defp encode_content(content) when is_binary(content), do: content
  defp encode_content(content), do: Jason.encode!(content)

  @doc """
  Deletes a conversation and all its events from the database.
  Returns updated history map without the deleted conversation.
  """
  def delete_conversation(history, id) do
    case Repo.get(Conversation, id) do
      nil -> :ok
      conversation -> Repo.delete!(conversation)
    end

    Map.delete(history, id)
  end

  @doc """
  Loads a conversation's events from the database.
  Returns {:ok, messages} or {:error, reason}.
  """
  def load_conversation(id) do
    case Repo.get(Conversation, id) do
      nil ->
        {:error, :not_found}

      _conversation ->
        events =
          Event
          |> where([e], e.conversation_id == ^id)
          |> order_by([e], asc: e.inserted_at)
          |> Repo.all()
          |> Enum.map(&event_to_map/1)

        {:ok, events}
    end
  end

  defp event_to_map(event) do
    %{
      "role" => event.role,
      "content" => decode_content(event.content),
      "model_id" => event.model_id,
      "inserted_at" => event.inserted_at
    }
  end

  defp decode_content(nil), do: nil

  defp decode_content(content) do
    case Jason.decode(content) do
      {:ok, decoded} -> decoded
      {:error, _} -> content
    end
  end

  @doc """
  Reads all conversations from the database.
  Returns a map of conversation id => title.
  """
  def read_history do
    Conversation
    |> order_by(desc: :updated_at)
    |> Repo.all()
    |> Enum.map(fn conv -> {conv.id, conv.title} end)
    |> Enum.into(Map.new())
  end

  defp get_first_message(conversation) do
    case conversation do
      [] -> "No messages"
      [%{"role" => "user", "content" => content} | _] -> content
      _ -> "No user message"
    end
  end

  defp fit_to_sql_string(title) do
    if String.length(title) > 250 do
      {prefix, _} = String.split_at(title, 250)
      prefix <> "..."
    else
      title
    end
  end
end
