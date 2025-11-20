defmodule Agents.Registry do
  use DynamicSupervisor

  def list_agents do
    for {:agent, name} <- :global.registered_names(), do: name
  end
end
