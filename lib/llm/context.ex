defmodule Llm.Context do
  defstruct id: nil,
            model_id: nil,
            messages: [],
            tools: [],
            process_tool_use: true,
            system_prompt: nil

  def create_context(context_id, model, system_prompt, tools) do
    selected_tools =
      Tools.ToolRegistry.get_all_tools()
      |> Enum.filter(fn tool -> tool.name() in tools end)

    %__MODULE__{
      id: context_id,
      model_id: model,
      messages: [],
      tools: selected_tools,
      process_tool_use: true,
      system_prompt: system_prompt
    }
  end

  def add_message(context, message) do
    %{context | messages: context.messages ++ [message]}
  end
end
