defmodule Llm.ModelRegistry do
  @moduledoc """
  A registry of models that can be used by an LLM.
  """

  alias Llm.Model

  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    models = [
      %Model{name: "Claude 3.5 Haiku - CRI", id: "us.anthropic.claude-3-5-haiku-20241022-v1:0", rate: 20},
      %Model{name: "Claude 4 Opus - CRI", id: "us.anthropic.claude-opus-4-20250514-v1:0", rate: 2},
      %Model{name: "Claude 4 Sonnet - CRI US", id: "us.anthropic.claude-sonnet-4-20250514-v1:0", rate: 2},
      %Model{name: "Claude 4 Sonnet - CRI EU", id: "eu.anthropic.claude-sonnet-4-20250514-v1:0", rate: 2},
      %Model{name: "Claude 3.7 Sonnet - CRI", id: "us.anthropic.claude-3-7-sonnet-20250219-v1:0", rate: 4},
      %Model{name: "Claude 3 Haiku", id: "anthropic.claude-3-haiku-20240307-v1:0", rate: 20},
      %Model{name: "Llama 4 Scout 17B", id: "us.meta.llama4-scout-17b-instruct-v1:0", rate: 20},
      %Model{name: "Llama 4 Maverick 17B", id: "us.meta.llama4-maverick-17b-instruct-v1:0", rate: 20},
      %Model{name: "Nova", id: "amazon.nova-pro-v1:0", rate: 10},
      %Model{name: "Nova Micro", id: "amazon.nova-micro-v1:0", rate: 10},
      %Model{name: "DeepSeek R1 - CRI", id: "us.deepseek.r1-v1:0", rate: 20}
    ]

    {:ok, %{models: models}}
  end

  def register_model(model_name, model_id) do
    GenServer.cast(__MODULE__, {:register_model, model_name, model_id})
  end

  def list_models() do
    GenServer.call(__MODULE__, :list_models)
  end

  @impl true
  def handle_cast({:register_model, model_name, model_id, rate}, state) do
    new_state = %{state | models: [%Model{name: model_name, id: model_id, rate: rate} | state.models]}
    {:noreply, new_state}
  end

  @impl true
  def handle_call(:list_models, _from, state) do
    {:reply, state.models, state}
  end
end
