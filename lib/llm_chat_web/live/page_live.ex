defmodule LlmChatWeb.PageLive do
  use LlmChatWeb, :live_view
  require Logger

  alias Llm.Bedrock # Module à créer pour l'interaction Bedrock

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        messages: [],           # Liste des messages: [%{role: :user | :assistant, content: "..."}]
        thoughts: [],           # Liste des pensées: [%{tool_use_id: "...", name: "...", input: %{...}, text: "..."}]
        current_input: "",
        total_tokens: 0,
        input_tokens: 0,
        output_tokens: 0,
        is_loading: false
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex h-screen font-sans">

      <div class="flex flex-col w-1/2 h-full border-r border-gray-300 bg-gray-50">
        <div class="flex-grow p-4 overflow-y-auto space-y-4">
          <%= for msg <- @messages do %>
            <div class={"flex " <> (if msg.role == :user, do: "justify-end", else: "justify-start")}>
              <div
                class={
                  "max-w-xl px-4 py-2 rounded-lg shadow " <>
                    if msg.role == :user do
                      "bg-blue-500 text-white"
                    else
                      "bg-white text-gray-800"
                    end
                }
              >
                <%= msg.content %>
              </div>
            </div>
          <% end %>
          <%= if @is_loading do %>
            <div class="flex justify-start">
              <div class="px-4 py-2 rounded-lg shadow bg-white text-gray-500 italic">
                Claude réfléchit...
              </div>
            </div>
          <% end %>
        </div>
        <div class="p-4 border-t border-gray-300 bg-white">
          <form phx-submit="send_message" phx-change="update_input" class="flex space-x-2">
            <input
              type="text"
              name="message"
              value={@current_input}
              placeholder="Envoyer un message à Claude..."
              class="flex-grow px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              autocomplete="off"
              phx-debounce="200"
            />
            <button
              type="submit"
              class="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
              disabled={@is_loading or String.trim(@current_input) == ""}
            >
              Envoyer
            </button>
          </form>
        </div>
      </div>

      <div class="flex flex-col w-1/2 h-full bg-gray-100">
        <div class="flex-grow p-4 overflow-y-auto space-y-4">
          <h2 class="text-lg font-semibold text-gray-700 mb-4">Pensées de Claude (Appels d'outils)</h2>
          <%= if Enum.empty?(@thoughts) do %>
             <p class="text-gray-500 italic">Aucune pensée pour le moment.</p>
          <% end %>
          <%= for thought <- @thoughts do %>
             <div class="p-3 rounded-lg bg-indigo-900 text-white shadow">
              <%= if thought.text do %>
               <p class="font-mono text-sm break-words"><%= thought.text %></p>
              <% else %>
               <p class="text-xs text-indigo-300 mt-1">Tool: <%= thought.name %>, input: <%= thought.input %></p>
              <% end %>
             </div>
          <% end %>
        </div>
        <div class="p-4 border-t border-gray-300 bg-white text-right">
          <span class="text-sm font-medium text-gray-600">
            <span class="font-bold text-indigo-700">Tokens utilisés : <%= @total_tokens %><br/>
            (input: <%= @input_tokens %>, output: <%= @output_tokens %>)</span>
          </span>
        </div>
      </div>
    </div>
    """
  end

  # --- Gestionnaires d'événements ---

  @impl true
  def handle_event("update_input", %{"message" => input}, socket) do
     {:noreply, assign(socket, :current_input, input)}
  end

  @impl true
  def handle_event("send_message", %{"message" => user_input}, socket) do
    trimmed_input = String.trim(user_input)
    if trimmed_input == "" or socket.assigns.is_loading do
      {:noreply, socket}
    else
      new_messages = socket.assigns.messages ++ [%{role: :user, content: trimmed_input}]

      # Mettre à jour l'UI immédiatement avec le message utilisateur
      socket =
        assign(socket,
          messages: new_messages,
          current_input: "",
          is_loading: true
        )

      bedrock_messages = Enum.map(new_messages, fn msg -> %{role: Atom.to_string(msg.role), content: msg.content} end)

      Bedrock.invoke(self(), bedrock_messages)

      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:bedrock_response, response_data}, socket) do
    messages = socket.assigns.messages ++ [%{role: :assistant, content: response_data.content}]

    socket =
      assign(socket,
        messages: messages,
        is_loading: false
      )

    {:noreply, socket}
  end

  @impl true
  def handle_info({:tokens_used, input_tokens, output_tokens}, socket) do
    socket =
      assign(socket,
        input_tokens: socket.assigns.input_tokens + input_tokens,
        output_tokens: socket.assigns.output_tokens + output_tokens,
        total_tokens: socket.assigns.total_tokens + input_tokens + output_tokens
      )

    {:noreply, socket}
  end

  @impl true
  def handle_info({:bedrock_error, error_details}, socket) do
    IO.inspect(error_details, label: "Bedrock Error")
    # Ajouter un message d'erreur à l'interface si désiré
    messages = socket.assigns.messages ++ [%{role: :assistant, content: "Désolé, une erreur s'est produite."}]

    socket = assign(socket, messages: messages, is_loading: false)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:bedrock_tool_use_only, response_data}, socket) do
     thoughts = socket.assigns.thoughts ++ response_data.thoughts

     socket =
       assign(socket,
         thoughts: thoughts,
         is_loading: false
       )
     {:noreply, socket}
  end

end
