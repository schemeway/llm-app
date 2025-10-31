defmodule LlmChatWeb.PageLive do
  use LlmChatWeb, :live_view
  require Logger

  import LlmChatWeb.Component.Toolbar, only: [toolbar: 1]
  import LlmChatWeb.Component.Conversation, only: [conversation: 1]
  import LlmChatWeb.Component.History, only: [history: 1]
  alias Llm.History
  alias Llm.PlatformRegistry
  alias Tools.ToolRegistry

  @system_prompt """
    You are an intelligent assistant. To answer the user's question, you can use the tools provided. Think step by step and
    verify your answer with the tools.

    If you are not sure about something, make sure to ask for clarification or use the appropriate tool to gather more information.

    You have access to the following information in your memory:
    {{memory}}

    Before running any tool that modifies the memory, make sure to check with the user if they want to proceed with the change.

    When you don't need any information to further process the user's request, just give your answer directly.
  """

  @impl true
  def mount(_params, _session, socket) do
    platform = PlatformRegistry.default_platform()
    model_id = platform.get_default_model_id()
    tool_names = ToolRegistry.get_tool_names()
    history = History.read_history()

    socket =
      assign(socket,
        system_prompt: @system_prompt,
        model_id: model_id,
        tools: tool_names,
        total_tokens: 0,
        input_tokens: 0,
        output_tokens: 0,
        platform: platform,
        history: history
      )
      |> initialize_conversation()

    {:ok, socket}
  end

  defp initialize_conversation(socket) do
    assign(socket,
      events: [],
      id: generate_conversation_id(),
      current_input: "",
      is_loading: false
    )
  end

  defp generate_conversation_id do
    UUID.uuid4()
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex w-100 h-screen font-sans">

      <.history conversations={@history} id={@id}/>

      <.conversation
        events={@events}
        current_input={@current_input}
        is_loading={@is_loading}
        phx_change="update_input"
        phx_submit="send_message"
        input_tokens={@input_tokens}
        output_tokens={@output_tokens}
        total_tokens={@total_tokens}
        system_prompt={@system_prompt}
      />

      <.toolbar platform={@platform} model_id={@model_id} tools={@tools} phx_change="update_model"/>


    </div>
    """
  end

  # --- Gestionnaires d'événements ---

  @impl true
  def handle_event("update_input", %{"message" => input}, socket) do
    {:noreply, assign(socket, :current_input, input)}
  end

  @impl true
  def handle_event("update_platform", %{"platform" => platform_name}, socket) do
    Logger.debug("Updating platform to: #{platform_name}")

    platform = PlatformRegistry.get_platform(platform_name)
    model_id = platform.get_default_model_id()

    socket = assign(socket, platform: platform, model_id: model_id)
    {:noreply, socket}
  end

  @impl true
  def handle_event("update_model", %{"model_id" => model_id}, socket) do
    socket = socket |> assign(:model_id, model_id)
    {:noreply, socket}
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
  def handle_event("enable_all_tools", _params, socket) do
    all_tools = ToolRegistry.get_tool_names()
    {:noreply, assign(socket, tools: all_tools)}
  end

  @impl true
  def handle_event("disable_all_tools", _params, socket) do
    {:noreply, assign(socket, tools: [])}
  end

  @impl true
  def handle_event("reset", _params, socket) do
    Logger.debug("Resetting the chat")
    socket = initialize_conversation(socket)
    {:noreply, socket}
  end

  @impl true
  def handle_event("send_message", message, socket) do
    user_input = extract_user_input(message)
    trimmed_input = String.trim(user_input)

    if trimmed_input == "" or socket.assigns.is_loading do
      {:noreply, socket}
    else
      new_messages = socket.assigns.events ++ [%{"role" => "user", "content" => trimmed_input}]

      # Mettre à jour l'UI immédiatement avec le message utilisateur
      socket =
        assign(socket,
          events: new_messages,
          current_input: "",
          is_loading: true
        )

      messages =
        new_messages
        |> Enum.filter(fn msg -> msg["role"] != "thought" end)

      memory = Memory.Store.list_keys() |> Enum.join(", ")
      system_prompt = String.replace(socket.assigns.system_prompt, "{{memory}}", memory)

      Logger.debug("System Prompt: #{system_prompt}")

      platform = socket.assigns.platform
      platform.invoke(
        self(),
        socket.assigns.model_id,
        system_prompt,
        socket.assigns.tools,
        messages
      )

      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("load_conversation", %{"id" => id}, socket) do
    Logger.debug("Loading conversation: #{id}")
    conversation = History.load_conversation(id)

    case conversation do
      {:ok, messages} ->
        socket = assign(socket, events: messages, id: id, is_loading: false, current_input: "")
        {:noreply, socket}

      {:error, reason} ->
        Logger.error("Failed to load conversation #{id}: #{reason}")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("delete_conversation", %{"id" => id}, socket) do
    new_history = History.delete_conversation(socket.assigns.history, id)
    socket = assign(socket, history: new_history)

    if socket.assigns.id == id do
      socket = initialize_conversation(socket)
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  defp extract_user_input(%{"message" => message}), do: message
  defp extract_user_input(%{"key" => "Enter", "value" => message}), do: message

  @impl true
  def handle_info({:llm_response, response_data}, socket) do
    events = socket.assigns.events ++ [response_data]

    socket =
      assign(socket,
        events: events,
        is_loading: false
      )
      |> push_event("focus_input", %{selector: "#message_input"})

    socket =
      assign(socket,
        history: History.save_conversation(socket.assigns.history, socket.assigns.id, socket.assigns.events)
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
  def handle_info({:llm_error, error_details}, socket) do
    Logger.error("LLM Error received: #{inspect(error_details)}")

    events =
      socket.assigns.events ++
        [%{role: :assistant, content: "Désolé, une erreur s'est produite."}]

    socket = assign(socket, events: events, is_loading: false)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:llm_tool_use_only, response_data}, socket) do
    events = socket.assigns.events ++ [response_data]

    socket =
      assign(socket,
        events: events,
        is_loading: false
      )

    {:noreply, socket}
  end
end
