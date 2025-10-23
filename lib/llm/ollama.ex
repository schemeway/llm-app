defmodule Llm.Ollama do
  alias Llm.OllamaClient
  alias Llm.Model
  use GenServer

  require Logger

  def name, do: "ollama"

  def get_models do
    GenServer.call(__MODULE__, :get_models)
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(callerid, model, system_prompt, tools, messages) do
    GenServer.cast(__MODULE__, {:invoke, callerid, model, system_prompt, tools, messages})
  end

  @impl true
  def init(:ok) do
    client = Ollama.init()
    models = retrieve_models(client)
    {:ok, %{client: client, models: models}}
  end

  @impl true
  def handle_call(:get_models, _from, %{models: models} = state) do
    {:reply, models, state}
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


  defp retrieve_models(client) do
    case Ollama.list_models(client) do
      {:ok, %{"models" => models}} ->
        for %{"name" => name, "model" => id} <- models do
          %Model{name: name, id: id, rate: 100}
        end

      {:error, reason} ->
        Logger.error("Erreur lors de la récupération des modèles Ollama: #{inspect(reason)}")
        []
    end
  end
end
