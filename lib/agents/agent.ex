defmodule Agents.Agent do
  use GenServer
  require Logger

  alias Llm.PlatformRegistry

  defstruct [:name, :description, :module, :model, :platform, :tools, :system_prompt]

  def start_link(options) do
    agent_name = Keyword.fetch!(options, :name)
    Logger.info("Starting agent: #{agent_name}")
    GenServer.start_link(__MODULE__, options, name: {:global, {:agent, agent_name}})
  end

  def stop(agent_name) do
    Logger.info("Stopping agent: #{agent_name}")
    GenServer.stop({:global, {:agent, agent_name}})
  end

  def send_message(agent_name, message) do
    GenServer.call({:global, {:agent, agent_name}}, {:send_message, message}, :infinity)
  end

  def init(options) do
    config = %__MODULE__{
      name: Keyword.fetch!(options, :name),
      description: Keyword.get(options, :description, ""),
      module: Keyword.get(options, :module, nil),
      model: Keyword.get(options, :model, nil),
      platform: Keyword.get(options, :platform, nil),
      tools: Keyword.get(options, :tools, []),
      system_prompt: Keyword.get(options, :system_prompt, nil)
    }

    {:ok, config}
  end

  def handle_call({:send_message, message}, _from, config) do
    response =
      if(config.module == nil) do
        process_message(config, message)
      else
        config.module.handle_message(config, message)
      end

    {:reply, response, config}
  end

  def process_message(config, message) do
    platform = config.platform || PlatformRegistry.default_platform()
    model = config.model || platform.get_default_model_id()
    tools = config.tools
    messages = [%{"role" => "user", "content" => message}]

    platform.invoke(self(), model, config.system_prompt, tools, messages)
    wait_for_response()
  end

  def wait_for_response() do
    receive do
      {:llm_response, %{"content" => message}} ->
        message

      _ ->
        wait_for_response()
    after
      30_000 ->
        :timeout
    end
  end
end

defmodule Agents.UserInput do
  def handle_message(_config, message) do
    IO.puts(message)
    IO.gets("> ")
  end
end

defmodule Agents.Test do
  def setup do
    {:ok, _} =
      Agents.Agent.start_link(
        name: "user_input",
        module: Agents.UserInput
      )

    {:ok, _} =
      Agents.Agent.start_link(
        name: "code_assistant",
        system_prompt: """
        You are an expert coding assistant.

        If you need to clarify the question because you don't have enough information, you can call 'user_input' agent using the 'AgentCall' tool.
        """,
        model: "amazon.nova-pro-v1:0",
        tools: ["DirReader", "FileReader", "FileWriter", "AgentCall"]
      )

    {:ok, _} =
      Agents.Agent.start_link(
        name: "financial_advisor",
        system_prompt: """
        You are a financial advisor bot. You can answer questions about finances, currencies, etc.
        """,
        model: "amazon.nova-pro-v1:0",
        tools: ["Calculator", "CurrencyRate"]
      )

    {:ok, _} =
      Agents.Agent.start_link(
        name: "orchestrator",
        system_prompt: """
        You are an orchestration agent, you dispatch requests to other agents. That is your only purpose. Do not try to answer the question yourself. Let the other agent clarify any information. 

        You must first decide to which agent you need to dispatch the message to. You do so by determining the category of the user's question.

        The agents you can dispatch messages to are:
        - financial_advisor: used for answering any type of questions regarging finances or currency rates, exchange rates, etc.
        - code_assistant: used for answering any type of questions related to programming, computer science.
        """,
        model: "amazon.nova-pro-v1:0",
        #        model: "anthropic.claude-3-haiku-20240307-v1:0",
        tools: ["AgentCall"]
      )
  end
end
