defmodule Memory.Store do
  use GenServer
  require Logger

  @moduledoc """
  A GenServer that manages an in-memory key-value store for storing textual information.
  """

  def start_link(_) do
    Logger.debug("Starting Memory.Store")
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

  @doc """
  Retrieves a value by key from the store.
  Returns {:ok, value} if the key exists, or {:error, :not_found} otherwise.
  """
  def get(key) when is_binary(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  @doc """
  Stores a key-value pair in the store.
  """
  def put(key, value) when is_binary(key) and is_binary(value) do
    GenServer.call(__MODULE__, {:put, key, value})
  end

  @doc """
  Lists all keys in the store.
  """
  def list_keys do
    GenServer.call(__MODULE__, :list_keys)
  end

  @doc """
  Deletes a key from the store.
  """
  def delete(key) when is_binary(key) do
    GenServer.call(__MODULE__, {:delete, key})
  end

  @doc """
  Clears all entries from the store.
  """
  def clear do
    GenServer.call(__MODULE__, :clear)
  end

  # Callbacks

  @impl true
  def handle_call({:get, key}, _from, state) do
    case Map.fetch(state, key) do
      {:ok, value} -> {:reply, {:ok, value}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:put, key, value}, _from, state) do
    new_state = Map.put(state, key, value)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:list_keys, _from, state) do
    keys = Map.keys(state)
    {:reply, keys, state}
  end

  @impl true
  def handle_call({:delete, key}, _from, state) do
    new_state = Map.delete(state, key)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:clear, _from, _state) do
    {:reply, :ok, %{}}
  end
end
