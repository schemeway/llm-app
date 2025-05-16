defmodule Llm.BedrockClient do
  require Logger

  alias Llm.{Message, Context}
  alias Tools.Tool

  defp notify_consumption(caller_pid, response) do
    case response do
      %{"usage" => %{"inputTokens" => input_tokes, "outputTokens" => output_tokens}} ->
        send(caller_pid, {:tokens_used, input_tokes, output_tokens})
    end
  end

  defp notify_thoughts(caller_pid, thought) do
    send(caller_pid, {:bedrock_tool_use_only, %{thoughts: [thought]}})
  end

  defp notify_answer(caller_pid, message) do
    send(caller_pid, {:bedrock_response, %{role: :assistant, content: message}})
  end

  def invoke(caller_pid, model, system_prompt, messages) do
    create_context(caller_pid, model, system_prompt)
    |> add_messages(messages)
    |> send_request()
  end

  defp create_context(caller_pid, model, system_prompt) do
    Context.new(model, system_prompt,
      caller_pid: caller_pid,
      tools: Tools.ToolRegistry.get_all_tools(),
      process_tools?: true
    )
  end

  defp add_messages(context, []), do: context

  defp add_messages(context, [%{role: "user", content: message} | messages]) do
    context
    |> Context.add_message(Message.user(message))
    |> add_messages(messages)
  end

  defp add_messages(context, [%{role: "assistant", content: message} | messages]) do
    context |> Context.add_message(Message.assistant(message)) |> add_messages(messages)
  end

  defp send_request(context) do
    Logger.debug(context.messages)

    {:ok, response} = invoke_bedrock(context)

    notify_consumption(context.caller_pid, response)

    process_response(response, context)
  end

  defp invoke_bedrock(context) do
    converse(context.model_id, context.messages, context.system_prompt, context.tools)
    |> ExAws.request(service_override: :bedrock)
  end

  defp process_response(
         %{
           "stopReason" => "end_turn",
           "output" => %{"message" => %{"content" => [%{"text" => text} | _]}}
         },
         context
       ) do
    notify_answer(context.caller_pid, text)
    :ok
  end

  defp process_response(
         %{
           "stopReason" => "tool_use",
           "output" => %{"message" => %{"content" => content} = message}
         },
         context
       ) do
    if context.process_tool_use do
      Logger.debug("Tool use: #{inspect(content)}")
      context = Context.add_message(context, message)
      process_tool_use(content, context, [])
    else
      content
    end
  end

  defp process_tool_use([], context, tool_results) do
    context
    |> Context.add_message(Message.tool_results(tool_results))
    |> send_request()
  end
  defp process_tool_use([%{"text" => text} | rest], context, tool_results) do
    notify_thoughts(context.caller_pid, %{text: text})
    process_tool_use(rest, context, tool_results)
  end
  defp process_tool_use([%{"toolUse" => %{}} = tool_use | rest], context, tool_results) do
    result = run_tool(context, tool_use)

    notify_thoughts(context.caller_pid, %{
      text: false,
      name: tool_use["toolUse"]["name"],
      input: Jason.encode!(tool_use["toolUse"]["input"])
    })

    process_tool_use(rest, context, tool_results ++ [result])
  end

  defp run_tool(context, %{
        "toolUse" => %{"name" => tool_name, "toolUseId" => tool_use_id, "input" => input}
      }) do
    tool = Enum.find(context.tools, fn tool -> tool.name() == tool_name end)

    tool.call(input)
    |> Message.tool_use(tool_use_id)
  end


  defp converse(model_id, messages, system_prompt, tools) do
    data =  %{
        # inferenceConfig: %{
        #   temperature: 1,
        #   topP: 1,
        # },
        # additionalModelRequestFields: %{
        #   inferenceConfig: %{
        #     topK: 1
        #   }
        # },
        max_tokens: 5_000,
        thinking: %{
          type: "enabled",
          budget_tokens: 2_000
        },
        messages: messages,
      }
      |> Map.merge(if system_prompt, do: %{system: [%{text: system_prompt}]}, else: %{})
      |> Map.merge(if tools, do: %{toolConfig: %{tools: tools |> Enum.map(&Tool.build_tool_spec/1)}}, else: %{})

    %ExAws.Operation.JSON{
      http_method: :post,
      headers: [{"Content-Type", "application/json"}],
      data: data,
      path: "/model/#{model_id}/converse",
      service: :"bedrock-runtime",
    }
  end
end
