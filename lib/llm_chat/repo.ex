defmodule LlmChat.Repo do
  use Ecto.Repo,
    otp_app: :llm_chat,
    adapter: Ecto.Adapters.Postgres
end
