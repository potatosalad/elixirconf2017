defmodule Contention.Handler do
  @behaviour :cowboy_handler

  def init(req, opts) do
    {__MODULE__, req, opts}
  end

  def upgrade(req, env, __MODULE__, _opts) do
    req =
      :cowboy_req.reply(200, %{
        "content-type" => "text/plain"
      }, "Hello world!", req)
    {:ok, req, Map.put_new(env, :result, :ok)}
  end

  def terminate(reason, _req, _stack) do
    :erlang.exit(reason)
  end
end