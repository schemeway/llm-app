defmodule Llm.Bedrock do
  alias Llm.BedrockClient
  alias Llm.Model
  use GenServer

  require Logger

  def name, do: "bedrock"

  def get_region do
    System.get_env("AWS_REGION") || "ca-central-1"
  end

  def get_models do
    GenServer.call(__MODULE__, :get_models)
  end

  def get_default_model_id do
    Application.get_env(:llm_chat, :default_bedrock_model, "anthropic-claude-haiku-4-5-20251001-v1:0")
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(callerid, model, system_prompt, tools, messages) do
    GenServer.cast(__MODULE__, {:invoke, callerid, model, system_prompt, tools, messages})
  end

  @impl true
  def init(:ok) do
    models = retrieve_models()
    {:ok, %{models: models}}
  end

  @impl true
  def handle_call(:get_models, _from, %{models: models} = state) do
    {:reply, models, state}
  end

  @impl true
  def handle_cast({:invoke, callerid, model, system_prompt, tools, messages}, state) do
    spawn_link(fn ->
      BedrockClient.invoke(callerid, model, system_prompt, tools, messages)  # Log the invocation
    end)

    {:noreply, state}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Message non traité reçu: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp retrieve_models() do

    on_demand_models() ++ inference_profile_models()
  end

  defp on_demand_models() do
    response =
      ExAws.Bedrock.list_foundation_models()
      |> ExAws.request(region: get_region())

    case response do
      {:ok, %{"modelSummaries" => models}} ->
        for %{"modelId" => id, "modelName" => name, "inferenceTypesSupported" => inference_types} <- models, "ON_DEMAND" in inference_types do
          Logger.info("Found Bedrock model: #{name} (ID: #{id})")
          %Model{name: name, id: id, rate: 2}
        end
      _ ->
        Logger.error("Erreur lors de la récupération des modèles Bedrock: #{inspect(response)}")
        []
    end
  end

  defp inference_profile_models() do
    response =
      %ExAws.Operation.JSON{http_method: :get, path: "/inference-profiles", service: :bedrock}
      |> ExAws.request(region: get_region())

    case response do
      {:ok, %{"inferenceProfileSummaries" => profiles}} ->
        for %{"inferenceProfileName" => name, "inferenceProfileId" => id} <- profiles do
          Logger.info("Found Bedrock inference profile: #{name} (Model ID: #{id})")
          %Model{name: name, id: id, rate: 125}
        end
      _ ->
        Logger.error("Erreur lors de la récupération des profils d'inférence Bedrock: #{inspect(response)}")
        []
    end
  end

end
