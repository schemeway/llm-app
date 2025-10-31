defmodule Tools.Memory do
  require Logger


  defmodule GetKey do
    use Tools.Tool,
      description: "Retrieves a piece of information from memory by key. Returns the stored value if the key exists.",
      parameters: %{
        "key" => %{
          "type" => "string",
          "description" => "The key to retrieve from memory"
        }
      }

    def call(%{"key" => key}) do
      Logger.debug("Retrieving memory for key: #{key}")

      case Memory.Store.get(key) do
        {:ok, value} ->
          %{"result" => value, "success" => true}

        {:error, :not_found} ->
          %{"success" => false, "error" => "Key not found in memory"}
      end
    end
  end

  defmodule PutKey do
    use Tools.Tool,
      description: "Saves a piece of textual information to memory with a given key. Overwrites existing values for the same key.",
      parameters: %{
        "key" => %{
          "type" => "string",
          "description" => "The key to store the information under"
        },
        "value" => %{
          "type" => "string",
          "description" => "The textual information to store"
        }
      }

    def call(%{"key" => key, "value" => value}) do
      Logger.debug("Saving to memory - key: #{key}")

      Memory.Store.put(key, value)
      %{"success" => true, "result" => "Information saved successfully"}
    end
  end

  defmodule DeleteKey do
    use Tools.Tool,
      description: "Deletes a piece of information from memory by key.",
      parameters: %{
        "key" => %{
          "type" => "string",
          "description" => "The key to delete from memory"
        }
      }

    def call(%{"key" => key}) do
      Logger.debug("Deleting memory for key: #{key}")

      case Memory.Store.delete(key) do
        :ok ->
          %{"success" => true, "result" => "Key deleted successfully"}

        {:error, :not_found} ->
          %{"success" => false, "error" => "Key not found in memory"}
      end
    end
  end

  defmodule Clear do
    use Tools.Tool,
      description: "Clears all entries from memory.",
      parameters: %{}

    def call(%{}) do
      Logger.debug("Clearing all memory")

      Memory.Store.clear()
      %{"success" => true, "result" => "All memory cleared successfully"}
    end
  end

  defmodule ListKeys do
    use Tools.Tool,
      description: "Lists all keys currently stored in memory.",
      parameters: %{}

    def call(%{}) do
      Logger.debug("Listing all memory keys")

      keys = Memory.Store.list_keys()
      %{"result" => keys}
    end
  end


end
