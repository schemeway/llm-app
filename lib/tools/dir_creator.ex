defmodule Tools.DirCreator do
  require Logger

  use Tools.Tool,
    description: "A tool for creating a directory.",
    parameters: %{
      "dir_path" => %{
        "type" => "string", 
        "description" => "the path to the directory to create"
      }
    }

  def call(%{"dir_path" => dir_path}) do
    Logger.debug("Attempting to create directory at #{dir_path}")

    case File.mkdir_p(dir_path) do
      :ok ->
        %{"result" => "Directory created successfully"}

      {:error, reason} ->
        Logger.error("Failed to create directory: #{reason}")
        %{"error" => "Failed to create directory: #{reason}"}
    end
  end
end