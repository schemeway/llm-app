defmodule LlmChat.Bedrock do
  alias LlmChat.BedrockClient
  use GenServer

  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(callerid, messages) do
    GenServer.cast(__MODULE__, {:invoke, callerid, messages})
  end

  @impl true
  def init(:ok) do
    # Initialize the Bedrock client here if needed
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:invoke, callerid, messages}, state) do
    spawn_link(fn ->
      BedrockClient.invoke(callerid, messages)  # Log the invocation
    end)

    {:noreply, state}
  end


  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Message non traité reçu: #{inspect(msg)}")
     # Gérer les messages non traités
    {:noreply, socket}
  end

end
