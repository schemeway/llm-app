defmodule LlmChatWeb.Toolbar do
  use Phoenix.Component

  attr :model_id, :string, default: "us.anthropic.claude-3-7-sonnet-20250219-v1:0"
  attr :tools, :list, default: []

  def toolbar(assigns) do
    ~H"""
      <div class="flex flex-col w-1/5 h-screen border-r border-gray-300 bg-gray-50">
      <.form phx-submit="update_model" class="p-4 h-dvh overflow-y-auto">
        <div>
          <h1 class="text-xl font-bold text-gray-800 mb-4">Model</h1>
          <select
            name="model_id"
            id="model_id"
            phx-change="update_model"
            class="w-full px-3 py-2 border rounded-md focus:outline-none focus:ring focus:ring-blue-500"
            >

            <%= for model <- Llm.ModelRegistry.list_models() do %>
              <option value={model.id} selected={model.id == @model_id}><%= model.name %></option>
            <% end %>
          </select>
        </div>

          <h1 class="text-xl font-bold text-gray-800 mt-4 mb-4">Tools</h1>

          <%= for tool <- Tools.ToolRegistry.get_all_tools() do %>
            <div class="mb-2">
              <label class="inline-flex items-center">
                <input
                  type="checkbox"
                  name="tools"
                  value={tool.name()}
                  checked={tool.name() in @tools}
                  phx-click="toggle_tool"
                  phx-value-tool={tool.name()}
                  class="form-checkbox h-4 w-4 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                />
                <span class="ml-2 text-gray-700"><%= tool.name() %></span>
              </label>
              <div class="text-xs text-gray-500 ml-2">
                  <%= tool.description() %>
              </div>
            </div>
          <% end %>
        </.form>

        <div class="p-4">
        <button
          class="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
          phx-click="reset">
          Reset
        </button>
        </div>

      </div>
    """
  end
end
