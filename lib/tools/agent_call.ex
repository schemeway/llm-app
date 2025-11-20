defmodule Tools.AgentCall do
  use Tools.Tool,
    description: "A tool for sending a message to an agent",
    parameters: %{
      "agent_name" => %{
        "type" => "string",
        "description" => "the name of the agent to call"
      },
      "message" => %{
        "type" => "string",
        "description" => "the message to send to the agent"
      }
    }

  def call(%{"agent_name" => name, "message" => message}) do
    response = Agents.Agent.send_message(name, message)
    %{"result" => response}
  end
end
