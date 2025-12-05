defmodule Llm.Notification do
  @moduledoc """
  Notification system for LLM events via Phoenix PubSub.
  """

  import Phoenix.PubSub, only: [broadcast: 3, subscribe: 2, unsubscribe: 2]

  def subscribe(context_id) do
    subscribe(LlmChat.PubSub, "context:" <> context_id)
  end

  def unsubscribe(context_id) do
    unsubscribe(LlmChat.PubSub, "context:" <> context_id)
  end

  def notify_consumption(context_id, response) do
    case response do
      %{"usage" => %{"inputTokens" => input_tokes, "outputTokens" => output_tokens}} ->
        notify_event(:llm_consumption, context_id, {input_tokes, output_tokens})
    end
  end

  def notify_thoughts(context_id, model_id, thought) do
    notify_event(:llm_thought, context_id, %{
      "role" => "thought",
      "content" => thought,
      "model_id" => model_id
    })
  end

  def notify_tool_use(context_id, tool_use) do
    notify_event(:llm_tool_use, context_id, %{"role" => "tool", "content" => tool_use})
  end

  def notify_answer(context_id, model_id, message) do
    notify_event(:llm_response, context_id, %{
      "role" => "assistant",
      "content" => message,
      "model_id" => model_id
    })
  end

  def notify_error(context_id, error_message) do
    notify_event(:llm_error, context_id, error_message)
  end

  defp notify_event(event, context_id, message) do
    broadcast(LlmChat.PubSub, "context:" <> context_id, {event, message})
  end
end
