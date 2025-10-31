defmodule LlmChatWeb.Component.Conversation do
  require Logger
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  defp format_input(nil), do: "nil"
  defp format_input(input) when is_binary(input) do
    input
  end
  defp format_input(input) when is_map(input) or is_list(input) do
    Jason.encode!(input, pretty: true, indent: "  ", line_separator: "\n")
  end

  def event(%{event: %{"role" => "thought"}} = assigns) do
    ~H"""
    <div class="p-3 mx-[50px] rounded-lg bg-indigo-900 text-white shadow">
      <%= if @event["content"]["text"] do %>
        <p class="font-mono text-xs break-words"><%= @event["content"]["text"] %></p>
      <% else %>
        <p class="font-mono text-xs text-indigo-300">
         <strong>Tool:</strong> "<%= @event["content"]["name"] %>" <br/>
         <strong>Input:</strong> <%= format_input(@event["content"]["input"]) %></p>
      <% end %>
    </div>
    """
  end

  def event(%{event: %{"role" => "tool"}} = assigns) do
    ~H"""
    <div class="p-3 mx-[50px] rounded-lg bg-indigo-900 text-indigo-300 shadow text-xs mt-1">
      <p class="font-mono text-xs break-words"><%= format_input(@event["content"]) %></p>
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

  def system_prompt(assigns) do
    ~H"""
    <div class="p-4 border-b border-gray-300 bg-white text-sm text-gray-600">
      <h3 class="font-bold mb-2">System Prompt:</h3>
      <pre class="whitespace-pre-wrap"><%= @system_prompt %></pre>
    </div>
    """
  end

  def button(assigns) do
    ~H"""
    <button
      class="shadow-lg px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-green-700 focus:outline-none focus:ring-2 focus:ring-green-500 disabled:opacity-50"
      phx-click={@phx_click}>
      <%= @label %>
    </button>
    """
  end

  def conversation(assigns) do
    ~H"""
    <div class="flex flex-col w-3/5 h-screen border-r border-gray-300 bg-gray-50">
        <div class="p-4 border-b border-gray-300 bg-white">
          <.button phx_click={JS.toggle_class("hidden", to: "#system-prompt")} label="Toggle System Prompt" />
          <.button phx_click="show_memory" label="Show Memory" />

          <span class="text-sm font-medium text-gray-600">
            <span class="font-bold text-indigo-700">Tokens used : <%= @total_tokens %>
            (input: <%= @input_tokens %>, output: <%= @output_tokens %>)</span>
            |
            <span class="font-bold"> Region: <%= System.get_env("AWS_REGION") || "ca-central-1" %></span>
          </span>
        </div>

      <div id="system-prompt" class="hidden">
        <.system_prompt system_prompt={@system_prompt} />
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
