 defmodule LlmChatWeb.Component.History do
  use LlmChatWeb, :live_component


  def history(assigns) do
    ~H"""
    <div class="history">
      <%= for {id, conversation} <- @conversations do %>
        <ul class="cursor-pointer hover:bg-gray-50 hover:rounded-xl border-b border-gray-300 py-2 truncate text-ellipsis" phx-click="load_conversation" phx-value-id={id}>
          <span class="font-semibold hover:text-blue-900 "><%= get_first_message(conversation) %></span>
          <!-- <span class="text-xs text-gray-500"><%= id %>:</span> -->
        </ul>
      <% end %>
    </div>
    """
  end

  def get_first_message(conversation) do
    case conversation do
      [] -> "No messages"
      [%{"role" => "user", "content" => content} | _] -> content
      _ -> "No user message"
    end
  end

end
