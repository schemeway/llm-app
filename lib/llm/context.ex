defmodule Llm.Context do
  defstruct model_id: nil,
            messages: [],
            tools: [],
            process_tool_use: true,
            caller_pid: nil,
            system_prompt: nil,
            client: nil

  def create_context(client, caller_pid, model, system_prompt, tools) do
    selected_tools =
      Tools.ToolRegistry.get_all_tools()
      |> Enum.filter(fn tool -> tool.name() in tools end)

    %__MODULE__{
      model_id: model,
      messages: [],
      tools: selected_tools,
      caller_pid: caller_pid,
      client: client,
      process_tool_use: true,
      system_prompt: system_prompt
    }
  end

  def add_message(context, message) do
    %{context | messages: context.messages ++ [message]}
  end
end
