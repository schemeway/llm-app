defmodule Tools.File do

  require Logger

  defmodule FileReader do

    use Tools.Tool,
      description: "A tool for reading the contents of a file.",
      parameters: %{
        "file_path" => %{
          "type" => "string",
          "description" => "the path to the file to read"
        }
      }

    def call(%{"file_path" => file_path}) do
      Logger.debug("Reading file at #{file_path}")

      case File.read(file_path) do
        {:ok, content} ->
          %{"content" => content}

        {:error, reason} ->
          Logger.error("Failed to read file: #{reason}")
          %{"error" => "Failed to read file: #{reason}"}
      end
    end
  end

  defmodule FileWriter do

    use Tools.Tool,
      description: "A tool for writing content to a file.",
      parameters: %{
        "file_path" => %{
          "type" => "string",
          "description" => "the path to the file to write"
        },
        "content" => %{
          "type" => "string",
          "description" => "the content to write to the file"
        }
      }

    def call(%{"file_path" => file_path, "content" => content}) do
      Logger.debug("Writing to file at #{file_path}")

      case File.write(file_path, content) do
        :ok ->
          %{"status" => "File written successfully"}

        {:error, reason} ->
          Logger.error("Failed to write file: #{reason}")
          %{"error" => "Failed to write file: #{reason}"}
      end
    end
  end
end
