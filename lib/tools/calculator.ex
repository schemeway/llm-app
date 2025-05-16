defmodule Tools.Calculator do
  require Logger

  use Tools.Tool,
    description: "A tool for performing simple arithmetic calculations, including a mix of operations like multiplication, division, addition, and subtraction.",
    parameters: %{
      "expression" => %{
        "type" => "string",
        "description" => "the full arithmetic expression to calculate"
      }
    }

  def call(%{"expression" => expression}) do
    Logger.debug("Calculating #{expression}")
    {result, _} = Code.eval_string(expression)

    %{"result" => result}
  end
end
