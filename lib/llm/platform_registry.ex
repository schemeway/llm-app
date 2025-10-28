defmodule Llm.PlatformRegistry do
  require Logger
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    platforms = Application.get_env(:llm_chat, :platforms, [Llm.Bedrock, Llm.Ollama])
    children = Enum.map(platforms, fn platform -> {platform, []} end)
    Supervisor.init(children, strategy: :one_for_one)
  end

  def get_platform(name) do
    Supervisor.which_children(__MODULE__)
    |> Enum.find(fn {mod, _, _, _} -> mod.name() == name end)
    |> case do
      nil -> nil
      {mod, _, _, _} -> mod
    end
  end

  def list_platforms do
    Supervisor.which_children(__MODULE__)
    |> Enum.map(fn {mod, _, _, _} -> mod.name() end)
  end

  def default_platform do
    Application.get_env(:llm_chat, :default_platform, Llm.Bedrock)
  end
end
