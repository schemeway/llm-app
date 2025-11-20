defmodule Agents.Artifact do
  defstruct [
    :id,
    :task_id,
    parts: []]

  defp artifact(id, task_id, parts) when is_list(parts) do
    %__MODULE__{id: id, task_id: task_id, parts: parts}
  end
end
