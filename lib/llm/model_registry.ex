defmodule Llm.ModelRegistry do
  @moduledoc """
  A registry of models that can be used by an LLM.
  """

  use GenServer

  alias LlmChat.BedrockClient
  alias LlmChat.Bedrock

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    # Initialize the model registry here if needed
    {:ok, %{models: []}}
  end

  def register_model(model_id) do
    GenServer.cast(__MODULE__, {:register_model, model_id})
  end

  def list_models() do
    GenServer.call(__MODULE__, :list_model)
  end

  @impl true
  def handle_cast({:register_model, model_id}, state) do
    new_state = %{state | models: [model_id | state.models]}
    {:noreply, new_state}
  end
  @impl true
  def handle_call(:list_model, _from, state) do
    {:reply, state.models, state}
  end
end
