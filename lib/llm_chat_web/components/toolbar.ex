defmodule LlmChatWeb.Component.Toolbar do
  use Phoenix.Component

  attr :platform, :string, required: true
  def platform_dropdown(assigns) do
    platform_name = assigns.platform.name()

    ~H"""
      <select
        name="platform"
        id="platform"
        phx-change="update_platform"
        class="w-full mb-3 px-3 py-2 border rounded-md focus:outline-none focus:ring focus:ring-blue-500"
      >
        <option :for={platform <- Llm.PlatformRegistry.list_platforms()}
          value={platform}
          selected={platform_name == platform}>
          <%= String.capitalize(platform) %>
        </option>
      </select>
    """
  end

  attr :model_id, :string, required: true
  attr :platform, :any, required: true
  def models_dropdown(assigns) do
    ~H"""
      <select
        name="model_id"
        id="model_id"
        phx-change="update_model"
        class="w-full px-3 py-2 border rounded-md focus:outline-none focus:ring focus:ring-blue-500"
      >
        <option :for={model <- @platform.get_models()}
          value={model.id}
          selected={model.id == @model_id}>
          <%= model.name %>
        </option>
      </select>
    """
  end

  attr :tool, :any, required: true
  attr :enabled_tools, :list, default: []

  def tool(assigns) do
    ~H"""
      <div class="mb-2">
        <label class="inline-flex items-center">
          <input
            type="checkbox"
            name="tools"
            value={@tool.name()}
            checked={@tool.name() in @enabled_tools}
            phx-click="toggle_tool"
            phx-value-tool={@tool.name()}
            class="form-checkbox h-4 w-4 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
          />
          <span class="ml-2 text-gray-700"><%= @tool.name() %></span>
        </label>
        <div class="text-xs text-gray-500 ml-2">
          <%= @tool.description() %>
        </div>
      </div>

    """
  end

  def tool_list(assigns) do
    ~H"""
      <.tool :for={tool <- Tools.ToolRegistry.get_all_tools()}
        tool={tool}
        enabled_tools={@tools} />
    """
  end

  def reset_button(assigns) do
    ~H"""
      <button
        class="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
        phx-click="reset">
        Reset
      </button>
    """
  end

  attr :title, :string, required: true
  def section(assigns) do
    ~H"""
      <h1 class="text-xl font-bold text-gray-800 mt-4 mb-4">{@title}</h1>
    """
  end


  attr :platform, :atom, required: true
  attr :model_id, :string, required: true
  attr :phx_change, :string, required: true
  attr :tools, :list, default: []

  def toolbar(assigns) do
    ~H"""
      <div class="flex flex-col w-1/5 h-screen border-r border-gray-300 bg-gray-50">
        <form phx-submit="{@phx_change}" class="p-4 h-dvh overflow-y-auto">
          <.section title="Model" />

          <.platform_dropdown platform={@platform} />

          <.models_dropdown model_id={@model_id} platform={@platform}/>

          <.section title="Tools" />

          <.tool_list tools={@tools} />
        </form>

        <div class="p-4 grid place-items-end">
          <.reset_button />
        </div>

      </div>
    """
  end
end
