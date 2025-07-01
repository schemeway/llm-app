defmodule Tools.ToolRegistry do
  use GenServer
  require Logger
  @moduledoc """
  A registry of tools that can be used by an LLM.
  """

  def start_link(_) do
    default_tools = [
      Tools.Dir.DirReader,
      Tools.Dir.DirCreator,
      Tools.File.FileReader,
      Tools.File.FileWriter,
    ]

    Agent.start_link(fn -> default_tools end, name: __MODULE__)
  end

  def init((init_arg)) do
    {:ok, init_arg}
  end

  def get_all_tools do
    Agent.get(__MODULE__, fn tools -> tools end)
  end

  def register_tool(tool) do
    Agent.update(__MODULE__, fn tools -> [tool | tools] end)
  end

  def get_tool(name) do
    Agent.get(__MODULE__, fn tools -> Enum.find(tools, fn tool -> tool.name() == name end) end)
  end

end