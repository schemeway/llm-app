defmodule Tools.TopicClassifier do
  use Tools.Tool,
    description: "A tool to validate the topic identified for the user input.",
    parameters: %{
      "topic" => %{
        "type" => "string",
        "description" => "the topic that has been identified"
      },
      "user_input" => %{
        "type" => "string",
        "description" => "the user input that was classified"
      },
      "confidence" => %{
        "type" => "number",
        "description" => "The confidence level of the classification."
      }
    }

  def call(%{"text" => text}) do
    # Dummy classification logic
    %{"classification" => text}
  end
end
