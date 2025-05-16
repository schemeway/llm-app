defmodule Llm.Context do

  defstruct [
    model_id: nil,
    messages: [],
    tools: [],
    process_tool_use: true,
    caller_pid: nil,
    system_prompt: nil
  ]

  def new(model_id, system_prompt, caller_pid: caller_pid, tools: tools, process_tools?: process_tools?) do
    %__MODULE__{
      model_id: model_id,
      messages: [],
      tools: tools || nil,
      caller_pid: caller_pid,
      process_tool_use: process_tools? && true,
      system_prompt: system_prompt
    }
  end

  def add_message(context, message) do
    %{context | messages: context.messages ++ [message]}
  end

end
