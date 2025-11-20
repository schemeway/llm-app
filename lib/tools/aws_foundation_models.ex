defmodule Tools.AwsFoundationModels do
  @moduledoc """
  A tool for listing all available foundation models in the user's AWS account via AWS Bedrock.
  """

  require Logger

  use Tools.Tool,
    description: "A tool for listing all available foundation models accessible in the user's AWS account via AWS Bedrock. Returns a list of models with their IDs, names, providers, and supported features.",
    parameters: %{
      "by_provider" => %{
        "type" => "string",
        "description" => "Optional filter to list models by provider (e.g., 'anthropic', 'amazon', 'meta', 'ai21', 'cohere', 'mistral'). If not specified, all models are returned."
      }
    }

  def call(params) do
    by_provider = Map.get(params, "by_provider")
    Logger.debug("Listing foundation models#{if by_provider, do: " for provider: #{by_provider}", else: ""}")

    region = System.get_env("AWS_REGION") || "ca-central-1"

    case list_foundation_models(region, by_provider) do
      {:ok, models} ->
        %{
          "status" => "success",
          "region" => region,
          "models" => models,
          "count" => length(models)
        }

      {:error, reason} ->
        Logger.error("Failed to list foundation models: #{inspect(reason)}")
        %{
          "status" => "error",
          "error" => "Failed to list foundation models: #{inspect(reason)}"
        }
    end
  end

  defp list_foundation_models(region, by_provider) do
    # Build the request
    params = build_params(by_provider)

    %ExAws.Operation.JSON{
      http_method: :get,
      headers: [{"Content-Type", "application/json"}],
      path: "/foundation-models",
      params: params,
      service: :bedrock
    }
    |> ExAws.request(service_override: :bedrock, region: region)
    |> case do
      {:ok, %{"modelSummaries" => model_summaries}} ->
        models = Enum.map(model_summaries, &format_model/1)
        {:ok, models}

      {:ok, response} ->
        Logger.warning("Unexpected response format: #{inspect(response)}")
        {:ok, []}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_params(nil), do: %{}
  defp build_params(provider), do: %{"byProvider" => provider}

  defp format_model(model_summary) do
    %{
      "model_id" => Map.get(model_summary, "modelId"),
      "model_name" => Map.get(model_summary, "modelName"),
      "provider_name" => Map.get(model_summary, "providerName"),
      "model_arn" => Map.get(model_summary, "modelArn"),
      "input_modalities" => Map.get(model_summary, "inputModalities", []),
      "output_modalities" => Map.get(model_summary, "outputModalities", []),
      "response_streaming_supported" => Map.get(model_summary, "responseStreamingSupported", false),
      "customizations_supported" => Map.get(model_summary, "customizationsSupported", []),
      "inference_types_supported" => Map.get(model_summary, "inferenceTypesSupported", [])
    }
  end
end
