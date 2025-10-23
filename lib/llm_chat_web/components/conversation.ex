defmodule LlmChatWeb.Component.Conversation do
  require Logger
  use Phoenix.Component

  defp format_input(nil), do: "nil"
  defp format_input(input) when is_binary(input) do
    input
  end
  defp format_input(input) when is_map(input) or is_list(input) do
    Jason.encode!(input)
  end

  def event(%{event: %{"role" => "thought"}} = assigns) do
    ~H"""
    <div class="p-3 mx-[50px] rounded-lg bg-indigo-900 text-white shadow">
      <%= if @event["content"]["text"] do %>
        <p class="font-mono text-xs break-words"><%= @event["content"]["text"] %></p>
      <% else %>
        <p class="text-xs text-indigo-300 mt-1">Tool: <%= @event["content"]["name"] %>, input: <%= format_input(@event["content"]["input"]) %></p>
      <% end %>
    </div>
    """
  end

  def event(%{event: %{"role" => "tool"}} = assigns) do
    ~H"""
    <div class="p-3 mx-[50px] rounded-lg bg-indigo-900 text-white shadow">
      <p class="font-mono text-xs break-words"><%= @event["content"] %></p>
    </div>
    """
  end

  def event(%{event: %{"role" => "user"}} = assigns) do
    ~H"""
    <div class="flex justify-end">
      <div class="max-w-xl px-4 py-2 rounded-lg bg-blue-500 text-white">
        {@event["content"]}
      </div>
    </div>
    """
  end

  def event(%{event: %{"role" => "assistant"}} = assigns) do
    ~H"""
    <div class="flex justify-start text-justify">
      <div class="max-w-xl px-4 py-2 rounded-lg bg-gray-50 text-gray-800 assistant">
        <%= {:safe, Earmark.as_html!(@event["content"], escape: true)} %>
      </div>
    </div>
    """
  end

  def cues(assigns) do
    ~H"""
    <%= if @events == [] do %>
      <div class="text-center text-gray-500 italic">
        Start a conversation by sending a message.
      </div>
    <% end %>
    <%= if @is_loading do %>
      <div class="text-center text-gray-500 italic">
        I'm thinking...
      </div>
    <% end %>
    """
  end

  def message(assigns) do
    ~H"""
      <div class="p-4 border-t border-gray-300 bg-white inset-x-0 bottom-0">
        <form phx-submit="send_message" id="message-form" phx-change="update_input" class="flex place-items-end space-x-2">
          <textarea
            style="field-sizing: content; min-height: 4rem; max-height: 10rem; resize: none;"
            id="message-input"
            type="text"
            name="message"
            value={@current_input}
            placeholder="Type your message..."
            class="flex-grow px-3 py-2  border-transparent resize-none rounded-md focus:border-transparent focus:ring-0 disabled:opacity-50"
            autocomplete="off"
            phx-debounce="200"
            disabled={@is_loading}
          />
          <script>
            const textarea = document.getElementById("message-input");
            const form = document.getElementById("message-form");

            document.addEventListener("DOMContentLoaded", function() {
              textarea.focus();
            });

            document.addEventListener("keydown", function(event) {
              if (event.key === "Enter" && !event.shiftKey) {
                event.preventDefault();
                form.focus();
                form.dispatchEvent(new Event("submit", { cancelable: true, bubbles: true }));
              }
            });

            window.addEventListener("phx:focus_input", () => {
              textarea.focus();
            });
          </script>

        </form>
      </div>
    """
  end

  def conversation(assigns) do
    ~H"""
    <div class="flex flex-col w-3/5 h-screen border-r border-gray-300 bg-gray-50">
        <div class="p-4 border-b border-gray-300 bg-white text-right">
          <span class="text-sm font-medium text-gray-600">
            <span class="font-bold text-indigo-700">Tokens used : <%= @total_tokens %>
            (input: <%= @input_tokens %>, output: <%= @output_tokens %>)</span>
            |
            <span class="font-bold"> Region: <%= System.get_env("AWS_REGION") || "ca-central-1" %></span>
          </span>
        </div>

      <div id="events" phx-hook="ScrollToBottom" class="flex-grow p-4 overflow-y-auto space-y-4">
        <.event :for={event <- @events} event={event} />
        <.cues is_loading={@is_loading} events={@events} />
      </div>

      <.message
        current_input={@current_input}
        is_loading={@is_loading} />


    </div>

    """
  end
end
