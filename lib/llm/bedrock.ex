defmodule Llm.Bedrock do
  alias Tools.Tool

  @model_id "amazon.nova-pro-01:0"
  def list_models(model \\ "eu.anthropic.claude-3-7-sonnet-20250219-v1:0") do
    # ExAws.Bedrock.list_foundation_models(by_provider: "anthropic")
    ExAws.Bedrock.get_foundation_model(model)
    |> IO.inspect()
    |> ExAws.request(service_override: :bedrock)
  end


  def converse(model_id, messages, system_prompt \\ nil, tools \\ nil) do
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
