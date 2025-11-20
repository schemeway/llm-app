defmodule Agents.Message do
  defstruct [
    :id,
    parts: []]

  def message(id, parts) when is_list(parts) do
    %__MODULE__{id: id, parts: parts}
  end
end
