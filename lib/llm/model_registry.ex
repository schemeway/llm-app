defmodule Llm.ModelRegistry do
  @moduledoc """
  A registry of models that can be used by an LLM.
  """

  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    models = [
      "amazon.nova-pro-v1:0",
      "arn:aws:bedrock:us-east-1:246675786323:inference-profile/us.anthropic.claude-sonnet-4-20250514-v1:0",
      "us.anthropic.claude-sonnet-4-20250514-v1:0",
      "us.anthropic.claude-3-7-sonnet-20250219-v1:0",
      "anthropic.claude-3-haiku-20240307-v1:0",
      "us.deepseek.r1-v1:0"
    ]

    {:ok, %{models: models}}
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
