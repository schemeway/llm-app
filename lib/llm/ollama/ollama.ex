defmodule Llm.Ollama do
  alias Llm.Ollama.Client
  alias Llm.Model
  use GenServer

  require Logger

  def name, do: "ollama"

  def get_models do
    GenServer.call(__MODULE__, :get_models)
  end

  def get_default_model_id do
    default_model = get_models() |> List.first() |> Map.get(:id)
    Application.get_env(:llm_chat, :default_ollama_model, default_model)
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(context_id, callerid, model, system_prompt, tools, messages) do
    GenServer.cast(
      __MODULE__,
      {:invoke, context_id, callerid, model, system_prompt, tools, messages}
    )
  end

  def embed_text(text) do
    GenServer.call(__MODULE__, {:embed_text, text})
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
  def handle_call({:embed_text, text}, _from, %{client: client} = state) do
    {:reply, Client.embed_text(client, text), state}
  end

  @impl true
  def handle_cast(
        {:invoke, context_id, callerid, model, system_prompt, tools, messages},
        %{client: client} = state
      ) do
    spawn_link(fn ->
      # Log the invocation
      Client.invoke(client, context_id, callerid, model, system_prompt, tools, messages)
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
    end
  end
end
