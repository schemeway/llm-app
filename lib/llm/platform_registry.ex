defmodule Llm.PlatformRegistry do
  @platforms %{
    Llm.Bedrock.name() => Llm.Bedrock,
    Llm.Ollama.name() => Llm.Ollama
  }

  def get_platform(name) do
    Map.get(@platforms, name)
  end

  def list_platforms do
    Map.keys(@platforms)
  end

  def default_platform do
    Application.get_env(:llm_chat, :default_platform, Llm.Bedrock)
  end
end
