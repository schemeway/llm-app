defmodule Llm.History do

  def save_conversation(history, id, messages) do
    dir = get_history_path()
    File.mkdir_p!(dir)
    Path.join(dir, "#{id}")
    |> File.write!(Jason.encode!(messages, pretty: true))

    Map.put(history, id, messages)
  end

  def delete_conversation(history, id) do
    dir = get_history_path()
    file_path = Path.join(dir, "#{id}")
    File.rm!(file_path)

    Map.delete(history, id)
  end

  def read_history do
    dir = get_history_path()

    File.ls!(dir)
    |> Enum.map(&read_file(dir, &1))
    |> Enum.into(Map.new())
  end

  defp read_file(dir, file_name) do
      {:ok, content} = File.read(Path.join(dir, file_name))
      {file_name, Jason.decode!(content)}
  end

  defp get_history_path() do
    System.get_env("HOME") <> "/.llm-app/data/history"
  end

end
