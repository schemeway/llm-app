defmodule Llm.Ollama.Client do
  require Logger

  alias Llm.Context
  import Llm.Notification

  def invoke(client, caller_pid, model, system_prompt, tools, messages) do
    Context.create_context(client, caller_pid, model, system_prompt, tools)
    |> add_messages(messages)
    |> send_request()
  end

  defp add_messages(context, []), do: context

  defp add_messages(context, [%{"role" => "user", "content" => message} | messages]) do
    context
    |> Context.add_message(%{role: "user", content: message})
    |> add_messages(messages)
  end

  defp add_messages(context, [%{"role" => "assistant", "content" => message} | messages]) do
    context
    |> Context.add_message(%{role: "assistant", content: message})
    |> add_messages(messages)
  end

  defp add_messages(context, [%{"role" => "tool", "content" => message} | messages]) do
    context
    |> Context.add_message(%{role: "tool", content: message})
    |> add_messages(messages)
  end

  defp send_request(context) do
    Logger.debug(context.messages)

    case invoke_ollama(context) do
      {:ok, response} ->
        Logger.debug("Ollama response: #{inspect(response)}")
        process_response(response, context)

      {:error, reason} ->
        Logger.error("Ollama invocation failed: #{inspect(reason)}")
        send(context.caller_pid, {:llm_error, reason})
    end
  end

  defp invoke_ollama(context) do
    Logger.debug("Invoking Ollama with model: #{context.model_id}")

    converse(
      context.client,
      context.model_id,
      context.messages,
      context.system_prompt,
      context.tools
    )
  end

  defp process_response(
         %{
           "message" =>
             %{
               "content" => content,
               "tool_calls" => tool_calls
             } = message
         },
         context
       ) do
    if context.process_tool_use do
      Logger.debug("Tool use: #{inspect(tool_calls)}")

      context =
        Context.add_message(context, %{
          role: message["role"],
          content: message["content"],
          tool_calls: message["tool_calls"]
        })

      process_tool_use(tool_calls, context)
    else
      content
    end
  end

  defp process_response(
         %{
           "message" => %{
             "content" => text,
             "role" => "assistant"
           }
         },
         context
       ) do
    notify_answer(context.caller_pid, text)
    :ok
  end

  defp process_tool_use([], context) do
    send_request(context)
  end

  defp process_tool_use([%{"text" => text} | rest], context) do
    notify_thoughts(context.caller_pid, %{text: text})
    process_tool_use(rest, context)
  end

  defp process_tool_use([%{"function" => tool_use} | rest], context) do
    result = run_tool(context, tool_use)

    notify_thoughts(context.caller_pid, %{
      "text" => false,
      "name" => tool_use["name"],
      "input" => tool_use["arguments"]
    })

    notify_tool_use(context.caller_pid, result["content"])

    context = Context.add_message(context, %{role: result["role"], content: result["content"]})
    process_tool_use(rest, context)
  end

  defp process_tool_use([unknown | rest], context) do
    Logger.warning("Unknown tool call: #{inspect(unknown)}")
    # Ignore unknown tool calls
    process_tool_use(rest, context)
  end

  defp run_tool(context, %{"name" => tool_name, "arguments" => input}) do
    tool = Enum.find(context.tools, fn tool -> tool.name() == tool_name end)

    Logger.debug("Running tool #{tool_name} with input: #{inspect(input)}")

    case tool.call(input) do
      %{"result" => result} ->
        Logger.debug("Tool #{tool_name} returned result: #{inspect(result)}")
        %{"role" => "tool", "content" => "#{result}"}

      %{"error" => error} ->
        %{"role" => "tool", "content" => "Error: #{error}"}
    end
  end

  defp converse(client, model_id, messages, system_prompt, tools) do
    tools =
      tools
      |> Enum.map(&Tools.Tool.build_tool_spec/1)
      |> Enum.map(&adapt_tool/1)

    messages = [
      %{role: "system", content: system_prompt || "You are a helpful assistant."}
      | messages
    ]

    Ollama.chat(client, model: model_id, messages: messages, tools: tools)
  end

  defp adapt_tool(%{toolSpec: spec}) do
    %{
      type: "function",
      function: %{
        name: spec.name,
        description: spec.description,
        parameters: spec.inputSchema.json
      }
    }
  end

  def embed_text(client, text) do
    {:ok, %{"embeddings" => [embedding]}} =
      Ollama.embed(client, input: text, model: "snowflake-arctic-embed:latest")

    embedding
  end
end
