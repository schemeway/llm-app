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
    [
      %Model{name: "Nova Micro - default", id: "amazon.nova-micro-v1:0", rate: 10},
      %Model{name: "Nova", id: "amazon.nova-pro-v1:0", rate: 10},
      %Model{name: "Claude 3 Haiku", id: "anthropic.claude-3-haiku-20240307-v1:0", rate: 20},
      %Model{name: "Claude 3.5 Haiku - CRI", id: "us.anthropic.claude-3-5-haiku-20241022-v1:0", rate: 20},
      %Model{name: "Claude 3.7 Sonnet - CRI", id: "us.anthropic.claude-3-7-sonnet-20250219-v1:0", rate: 4},
      %Model{name: "Claude 4 Sonnet - CRI US", id: "us.anthropic.claude-sonnet-4-20250514-v1:0", rate: 2},
      %Model{name: "Claude 4 Sonnet - CRI EU", id: "eu.anthropic.claude-sonnet-4-20250514-v1:0", rate: 2},
      %Model{name: "Claude 4.5 Sonnet - CRI US", id: "us.anthropic.claude-sonnet-4-5-20250929-v1:0", rate: 2},
      %Model{name: "Claude 4.5 Sonnet - CRI Global", id: "global.anthropic.claude-sonnet-4-5-20250929-v1:0", rate: 2},
      %Model{name: "Llama 4 Scout 17B", id: "us.meta.llama4-scout-17b-instruct-v1:0", rate: 20},
      %Model{name: "Llama 4 Maverick 17B", id: "us.meta.llama4-maverick-17b-instruct-v1:0", rate: 20},
      %Model{name: "DeepSeek R1 - CRI", id: "us.deepseek.r1-v1:0", rate: 20},
    ]
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def invoke(callerid, model, system_prompt, tools, messages) do
    GenServer.cast(__MODULE__, {:invoke, callerid, model, system_prompt, tools, messages})
  end

  @impl true
  def init(:ok) do
    # Initialize the Bedrock client here if needed
    {:ok, %{}}
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
     # Gérer les messages non traités
    {:noreply, socket}
  end

end
