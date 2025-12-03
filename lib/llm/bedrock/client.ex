defmodule Llm.Bedrock.Client do
  require Logger

  alias Llm.{Bedrock, Context, Message}
  alias Tools.Tool

  defp notify_consumption(caller_pid, response) do
    case response do
      %{"usage" => %{"inputTokens" => input_tokes, "outputTokens" => output_tokens}} ->
        send(caller_pid, {:tokens_used, input_tokes, output_tokens})
    end
  end

  defp notify_thoughts(caller_pid, thought) do
    send(caller_pid, {:llm_tool_use_only, %{"role" => "thought", "content" => thought}})
  end

  defp notify_tool_use(caller_pid, tool_use) do
    send(caller_pid, {:llm_tool_use_only, %{"role" => "tool", "content" => tool_use}})
  end

  defp notify_answer(caller_pid, message) do
    send(caller_pid, {:llm_response, %{"role" => "assistant", "content" => message}})
  end

  def invoke(caller_pid, model, system_prompt, tools, messages) do
    create_context(caller_pid, model, system_prompt, tools)
    |> add_messages(messages)
    |> send_request()
  end

  defp create_context(caller_pid, model, system_prompt, tools) do
    selected_tools =
      Tools.ToolRegistry.get_all_tools()
      |> Enum.filter(fn tool -> tool.name() in tools end)

    Context.new(model, system_prompt,
      caller_pid: caller_pid,
      tools: selected_tools,
      process_tools?: true
    )
  end

  defp add_messages(context, []), do: context

  defp add_messages(context, [%{"role" => "user", "content" => message} | messages]) do
    context
    |> Context.add_message(Message.user(message))
    |> add_messages(messages)
  end

  defp add_messages(context, [%{"role" => "assistant", "content" => message} | messages]) do
    context |> Context.add_message(Message.assistant(message)) |> add_messages(messages)
  end

  defp add_messages(context, [_ | messages]) do
    # we just skip the tool calls and results
    context |> add_messages(messages)
  end

  defp send_request(context) do
    Logger.debug(context.messages)

    case invoke_bedrock(context) do
      {:ok, response} ->
        notify_consumption(context.caller_pid, response)
        process_response(response, context)

      {:error, {:http_error, 429, _}} ->
        Logger.warning("Bedrock invocation rate limit exceeded, retrying in 30 seconds...")
        Process.sleep(30_000)
        send_request(context)

      {:error, reason} ->
        Logger.error("Bedrock invocation failed: #{inspect(reason)}")
        send(context.caller_pid, {:bedrock_error, reason})
    end
  end

  defp invoke_bedrock(context) do
    Logger.debug("Invoking Bedrock with model: #{context.model_id}")

    converse(context.model_id, context.messages, context.system_prompt, context.tools)
    |> ExAws.request(service_override: :bedrock, region: Bedrock.get_region())
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
    notify_thoughts(context.caller_pid, %{"text" => text})
    process_tool_use(rest, context, tool_results)
  end

  defp process_tool_use([%{"toolUse" => %{}} = tool_use | rest], context, tool_results) do
    result = run_tool(context, tool_use)

    notify_thoughts(context.caller_pid, %{
      "text" => false,
      "name" => tool_use["toolUse"]["name"],
      "input" => Jason.encode!(tool_use["toolUse"]["input"])
    })

    notify_tool_use(context.caller_pid, result)

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
    data =
      %{
        anthropic_version: "bedrock-2023-05-31",
        max_tokens: 1_024,
        max_tokens_to_sample: 1_024,
        thinking: %{
          type: "enabled",
          budget_tokens: 2_000
        },
        messages: messages
      }
      |> Map.merge(if system_prompt, do: %{system: [%{text: system_prompt}]}, else: %{})
      |> Map.merge(
        if is_list(tools) and not (tools == []),
          do: %{toolConfig: %{tools: tools |> Enum.map(&Tool.build_tool_spec/1)}},
          else: %{}
      )

    Logger.debug("Bedrock request data:\n#{Jason.encode!(data, pretty: true)}")

    %ExAws.Operation.JSON{
      http_method: :post,
      headers: [{"Content-Type", "application/json"}],
      data: data,
      path: "/model/#{model_id}/converse",
      service: :"bedrock-runtime"
    }
  end

  def embed_text(text) do
    data = %{inputText: text}

    Logger.debug("Bedrock embedding request data:\n#{Jason.encode!(data, pretty: true)}")

    %ExAws.Operation.JSON{
      http_method: :post,
      headers: [{"Content-Type", "application/json"}],
      data: data,
      path: "/model/amazon.titan-embed-text-v1/invoke",
      service: :"bedrock-runtime"
    }
    |> ExAws.request(service_override: :bedrock, region: Bedrock.get_region())
    |> case do
      {:ok, response} ->
        response["embedding"]

      {:error, reason} ->
        Logger.error("Bedrock embedding failed: #{inspect(reason)}")
        {:error, reason}
    end
  end
end
