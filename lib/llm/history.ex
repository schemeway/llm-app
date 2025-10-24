defmodule Llm.History do
  require Logger

  def save_conversation(history, id, messages) do
    Logger.debug("Saving conversation #{id} with #{length(messages)} messages.")

    dir = get_history_path()
    File.mkdir_p!(dir)
    Path.join(dir, "#{id}")
    |> File.write!(Jason.encode!(messages, pretty: true))

    Map.put(history, id, messages |> get_first_message())
  end

  def delete_conversation(history, id) do
    dir = get_history_path()
    file_path = Path.join(dir, "#{id}")
    File.rm!(file_path)

    Map.delete(history, id)
  end

  def load_conversation(id) do
    dir = get_history_path()
    file_path = Path.join(dir, "#{id}")

    case File.read(file_path) do
      {:ok, content} -> {:ok, Jason.decode!(content)}
      {:error, reason} -> {:error, reason}
    end
  end

  def read_history do
    dir = get_history_path()

    File.ls!(dir)
    |> Enum.map(&read_description(dir, &1))
    |> Enum.into(Map.new())
  end

  defp read_description(dir, file_name) do
      {:ok, content} = File.read(Path.join(dir, file_name))
      description = get_first_message(Jason.decode!(content))
      {file_name, description}
  end

  defp get_history_path() do
    System.get_env("HOME") <> "/.llm-app/data/history"
  end

  defp get_first_message(conversation) do
    case conversation do
      [] -> "No messages"
      [%{"role" => "user", "content" => content} | _] -> content
      _ -> "No user message"
    end
  end


end
