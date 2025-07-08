defmodule LlmChatWeb.PageLive do
  use LlmChatWeb, :live_view
  require Logger

  # Module à créer pour l'interaction Bedrock
  alias Llm.Bedrock
  import LlmChatWeb.Component.Toolbar, only: [toolbar: 1]
  import LlmChatWeb.Component.Conversation, only: [conversation: 1]

  @system_prompt """
    You are an intelligent assistant. To answer the user's question, you can use the tools provided. Think step by step and
    verify your answer with the tools.

  When you don't need any information to further process the user's request, just answer by writing
  Final answer: <answer>
  """

  @impl true
  def mount(_params, _session, socket) do
    default_model = Llm.ModelRegistry.list_models() |> List.first() |> Map.get(:id)

    socket =
      assign(socket,
        system_prompt: @system_prompt,
        model_id: default_model,
        tools: Tools.ToolRegistry.get_all_tools() |> Enum.map(& &1.name()),
        total_tokens: 0,
        input_tokens: 0,
        output_tokens: 0
      )
      |> initialize()

    {:ok, socket}
  end

  defp initialize(socket) do
    assign(socket,
      events: [],
      current_input: "",
      is_loading: false
    )
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex w-100 h-screen font-sans">

      <.toolbar model_id={@model_id} tools={@tools} phx_change="update_model"/>

      <.conversation
        events={@events}
        current_input={@current_input}
        is_loading={@is_loading}
        phx_change="update_input"
        phx_submit="send_message"
      />

      <div class="flex flex-col w-2/5 h-screen bg-gray-100">
        <div class="flex-grow p-4 overflow-y-auto space-y-4">
          <h1 class="text-xl font-bold text-gray-800 mb-4">Conversations</h1>
        </div>
        <div class="p-4 border-t border-gray-300 bg-white text-right">
          <span class="text-sm font-medium text-gray-600">
            <span class="font-bold text-indigo-700">Tokens used : <%= @total_tokens %>
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
  def handle_event("update_model", %{"model_id" => model_id}, socket) do
    if Llm.ModelRegistry.list_models()
       |> Enum.find(fn model -> model.id == model_id end) do
      socket = assign(socket, model_id: model_id)
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("toggle_tool", %{"tool" => tool_name}, socket) do
    tools = socket.assigns.tools

    Logger.debug("Toggling tool: #{tool_name}")

    if tool_name in tools do
      new_tools = List.delete(tools, tool_name)
      {:noreply, assign(socket, tools: new_tools)}
    else
      new_tools = tools ++ [tool_name]
      {:noreply, assign(socket, tools: new_tools)}
    end
  end

  @impl true
  def handle_event("reset", _params, socket) do
    Logger.debug("Resetting the chat")
    socket = initialize(socket)
    {:noreply, socket}
  end

  @impl true
  def handle_event("send_message", %{"message" => user_input}, socket) do
    trimmed_input = String.trim(user_input)

    if trimmed_input == "" or socket.assigns.is_loading do
      {:noreply, socket}
    else
      new_messages = socket.assigns.events ++ [%{role: :user, content: trimmed_input}]

      # Mettre à jour l'UI immédiatement avec le message utilisateur
      socket =
        assign(socket,
          events: new_messages,
          current_input: "",
          is_loading: true
        )

      bedrock_messages =
        new_messages
        |> Enum.filter(fn msg -> msg.role != :thought end)
        |> Enum.map(fn msg ->
          %{role: Atom.to_string(msg.role), content: msg.content}
        end)

      Bedrock.invoke(
        self(),
        socket.assigns.model_id,
        socket.assigns.system_prompt,
        socket.assigns.tools,
        bedrock_messages
      )

      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:bedrock_response, response_data}, socket) do
    events = socket.assigns.events ++ [%{role: :assistant, content: response_data.content}]

    socket =
      assign(socket,
        events: events,
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
    events =
      socket.assigns.events ++
        [%{role: :assistant, content: "Désolé, une erreur s'est produite."}]

    socket = assign(socket, events: events, is_loading: false)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:bedrock_tool_use_only, response_data}, socket) do
    events = socket.assigns.events ++ [response_data]

    socket =
      assign(socket,
        events: events,
        is_loading: false
      )

    {:noreply, socket}
  end
end
