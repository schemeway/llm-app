defmodule Agents.Registry do
  use DynamicSupervisor

  def start_link(init_args) do
    DynamicSupervisor.start_link(__MODULE__, init_args, name: __MODULE__)
  end

  @impl true
  def init(_init_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def list_agents do
    for {:agent, name} <- :global.registered_names(),
        do: name
  end

  def start_agent(options) do
    DynamicSupervisor.start_child(__MODULE__, {Agents.Agent, options})
  end
end
