defmodule Llm.Message do
  @user_role "user"
  @assistant_role "assistant"

  def user(text) do
    %{
      role: @user_role,
      content: [%{text: text}]
    }
  end

  def assistant(text) do
    %{
      role: @assistant_role,
      content: [%{text: text}]
    }
  end

  def tool_use(result, tool_use_id) do
    %{
      toolResult: %{
        toolUseId: tool_use_id,
        content: [%{"json" => result}],
        status: "success"
      }
    }
  end

   @spec tool_results([any()]) :: %{content: [any()], role: <<_::32>>}
   def tool_results(results) do
    %{
      role: "user",
      content: results
    }
  end
end
