defmodule Tools.DirReader do
  require Logger

  use Tools.Tool,
    description: "A tool for reading the contents of a directory.",
    parameters: %{
      "dir_path" => %{
        "type" => "string",
        "description" => "the path to the directory to read"
      }
    }

  def call(%{"dir_path" => dir_path}) do
    Logger.debug("Reading directory at #{dir_path}")

    case File.ls(dir_path) do
      {:ok, files} ->
        %{"files" => files}

      {:error, reason} ->
        Logger.error("Failed to read directory: #{reason}")
        %{"error" => "Failed to read directory: #{reason}"}
    end
  end
end
