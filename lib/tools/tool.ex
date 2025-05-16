defmodule Tools.Tool do
  @moduledoc """
  A tool is a function that can be called by an LLM.
  """
  # @callback name() :: String.t()

  # @callback description() :: String.t()

  # @callback parameters() :: Map.t()

  @callback call(map()) :: Map.t()


  def build_tool_spec(tool) do
    %{
      toolSpec: %{
        name: tool.name(),
        description: tool.description(),
        inputSchema: %{
          json: %{
            type: "object",
            properties: tool.parameters(),
            required: tool.parameters() |> Map.keys()
          }
        }
      }
    }
  end

  defmacro __using__(opts) do
    name =
      Module.split(__CALLER__.module)
      |> List.last()

    quote do
      @behaviour unquote(__MODULE__)

      @description unquote(opts[:description])
      @parameters unquote(opts[:parameters])

      def name, do: unquote(name)
      def description, do: @description
      def parameters, do: @parameters
    end
  end
end
