defmodule Agents.Part do
  defstruct [
    :type, # one of :text, :image, :file
    content: nil
  ]

  def text_part(content) when is_binary(content) do
    %__MODULE__{type: :text, content: content}
  end

  def file_part(:uri, uri, mime_type) when is_binary(uri) do
    %__MODULE__{type: :file, content: %{type: :uri, uri: uri, mime_type: mime_type}}
  end

  def file_part(:binary, bytes, mime_type) when is_binary(bytes) do
    %__MODULE__{type: :file, content: %{type: :binary, data: bytes, mime_type: mime_type}}
  end

  def data_part(data) when is_binary(data) do
    %__MODULE__{type: :data, content: %{data: data}}
  end
end
