 defmodule LlmChatWeb.Component.History do
  use LlmChatWeb, :live_component


  def history(assigns) do
    ~H"""
      <div class="flex flex-col w-1/5 h-screen bg-gray-200">
        <div class="flex-grow p-4 overflow-y-auto space-y-4">
          <h1 class="text-xl font-bold text-gray-800 mb-4">Conversations</h1>
          <div class="history">
            <%= for {id, description} <- @conversations do %>
              <div class="history-item relative group">
                <ul class="cursor-pointer hover:bg-gray-50 hover:rounded-xl border-b border-gray-300 py-2 truncate text-ellipsis" phx-click="load_conversation" phx-value-id={id}>
                  <span class="font-semibold hover:text-blue-900 "><%= description %></span>
                  <br/> <span class="text-xs text-gray-500"><%= id %></span>
                </ul>
                <button
                  class="absolute right-2 top-2 opacity-0 group-hover:opacity-100 transition-opacity bg-gray-500 text-white rounded-full w-6 h-6 flex items-center justify-center text-xs hover:bg-gray-600"
                  phx-click="delete_conversation"
                  phx-value-id={id}
                >x</button>
              </div>
            <% end %>
          </div>
        </div>
      </div>
    """
  end

end
