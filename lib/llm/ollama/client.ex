defmodule Llm.Ollama.Client do
  require Logger

  alias Llm.Context
  import Llm.Notification

  def invoke(client, context_id, model, system_prompt, tools, messages) do
    Context.create_context(context_id, model, system_prompt, tools)
    |> add_messages(messages)
    |> send_request(client)
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

  defp send_request(context, client) do
    Logger.debug(context.messages)

    case invoke_ollama(context, client) do
      {:ok, response} ->
        Logger.debug("Ollama response: #{inspect(response)}")
        process_response(response, context, client)

      {:error, reason} ->
        Logger.error("Ollama invocation failed: #{inspect(reason)}")
        notify_error(context.id, reason)
    end
  end

  defp invoke_ollama(context, client) do
    Logger.debug("Invoking Ollama with model: #{context.model_id}")

    converse(
      client,
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
         context,
         client
       ) do
    if context.process_tool_use do
      Logger.debug("Tool use: #{inspect(tool_calls)}")

      context =
        Context.add_message(context, %{
          role: message["role"],
          content: message["content"],
          tool_calls: message["tool_calls"]
        })

      process_tool_use(tool_calls, context, client)
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
         context,
         _client
       ) do
    notify_answer(context.id, text)
    :ok
  end

  defp process_tool_use([], context, client) do
    send_request(context, client)
  end

  defp process_tool_use([%{"text" => text} | rest], context, client) do
    notify_thoughts(context.id, %{text: text})
    process_tool_use(rest, context, client)
  end

  defp process_tool_use([%{"function" => tool_use} | rest], context, client) do
    result = run_tool(context, tool_use)

    notify_thoughts(context.id, %{
      "text" => false,
      "name" => tool_use["name"],
      "input" => tool_use["arguments"]
    })

    notify_tool_use(context.id, result["content"])

    context = Context.add_message(context, %{role: result["role"], content: result["content"]})
    process_tool_use(rest, context, client)
  end

  defp process_tool_use([unknown | rest], context, client) do
    Logger.warning("Unknown tool call: #{inspect(unknown)}")
    # Ignore unknown tool calls
    process_tool_use(rest, context, client)
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
