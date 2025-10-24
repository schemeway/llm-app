defmodule Tools.Dir do
  require Logger

  defmodule DirCreator do
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

  defmodule DirReader do
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
          %{"result" => files}

        {:error, reason} ->
          Logger.error("Failed to read directory: #{reason}")
          %{"error" => "Failed to read directory: #{reason}"}
      end
    end
  end

  defmodule DirChanger do
    use Tools.Tool,
      description: "A tool for changing the current working directory.",
      parameters: %{
        "dir_path" => %{
          "type" => "string",
          "description" => "the path to the directory to change to"
        }
      }

    def call(%{"dir_path" => dir_path}) do
      Logger.debug("Attempting to change directory to #{dir_path}")

      case File.cd(dir_path) do
        :ok ->
          current_dir = File.cwd!()
          Logger.info("Successfully changed directory to #{current_dir}")
          %{"result" => "Directory changed successfully", "current_directory" => current_dir}

        {:error, reason} ->
          Logger.error("Failed to change directory: #{reason}")
          %{"error" => "Failed to change directory: #{reason}"}
      end
    end
  end
end
