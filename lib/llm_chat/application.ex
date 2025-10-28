defmodule LlmChat.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  @spec start(any(), any()) :: {:error, any()} | {:ok, pid()}
  def start(_type, _args) do

    children = [
      LlmChatWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:llm_chat, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: LlmChat.PubSub},
      Llm.PlatformRegistry,
      Tools.ToolRegistry,
      # Start the Finch HTTP client for sending emails
      {Finch, name: LlmChat.Finch},
      # Start a worker by calling: LlmChat.Worker.start_link(arg)
      # {LlmChat.Worker, arg},
      # Start to serve requests, typically the last entry
      LlmChatWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: LlmChat.Supervisor]
    Supervisor.start_link(children, opts)
  end


  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    LlmChatWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
