defmodule Llm.History do

  def save_conversation(history, id, messages) do
    dir = File.cwd!() <> "/data/history"
    File.mkdir_p!(dir)
    file_path = Path.join(dir, "#{id}")
    File.write!(file_path, Jason.encode!(messages, pretty: true))
    Map.put(history, id, messages)
  end

  def read_history do
    dir = File.cwd!() <> "/data/history"

    File.ls!(dir)
    |> Enum.map(fn file ->
      {:ok, content} = File.read(Path.join(dir, file))
      {file, Jason.decode!(content)}
    end)
    |> Enum.into(Map.new())
  end

end
