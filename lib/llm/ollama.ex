defmodule Llm.Ollama do
  alias Llm.OllamaClient
  use GenServer

  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(callerid, model, system_prompt, tools, messages) do
    GenServer.cast(__MODULE__, {:invoke, callerid, model, system_prompt, tools, messages})
  end

  @impl true
  def init(:ok) do
    # Initialize the Ollama client here if needed
    {:ok, %{client: Ollama.init()}}
  end

  @impl true
  def handle_cast({:invoke, callerid, model, system_prompt, tools, messages}, %{client: client} = state) do
    spawn_link(fn ->
      OllamaClient.invoke(client, callerid, model, system_prompt, tools, messages)  # Log the invocation
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
