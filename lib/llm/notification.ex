defmodule Llm.Notification do
  def notify_consumption(caller_pid, response) do
    case response do
      %{"usage" => %{"inputTokens" => input_tokes, "outputTokens" => output_tokens}} ->
        send(caller_pid, {:tokens_used, input_tokes, output_tokens})
    end
  end

  def notify_thoughts(caller_pid, thought) do
    send(caller_pid, {:llm_tool_use_only, %{"role" => "thought", "content" => thought}})
  end

  def notify_tool_use(caller_pid, tool_use) do
    send(caller_pid, {:llm_tool_use_only, %{"role" => "tool", "content" => tool_use}})
  end

  def notify_answer(caller_pid, message) do
    send(caller_pid, {:llm_response, %{"role" => "assistant", "content" => message}})
  end
end
