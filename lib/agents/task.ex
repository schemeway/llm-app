defmodule Agents.Task do
  defstruct [
    :id,
    :name,
    :state, # one of :submitted, :working, :input_required, :completed, :failed, :cancelled
    :messages,
    :history,
  ]


end
