defmodule Llm.Platform do


  @callback name() :: String.t()

  @callback initialize_models(state :: any()) :: [Llm.Model.t()]
  @callback get_default_model_id(state :: any()) :: String.t()

  @callback handle_invocation(
              callerid :: String.t(),
              model :: Llm.Model.t(),
              system_prompt :: String.t(),
              tools :: [Tools.Tool.t()],
              messages :: [Llm.Message.t()],
              state :: any()
  ) :: :ok


  defmacro __using__(opts) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)

      def start_link(_) do
        GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
      end



    end
  end
end
