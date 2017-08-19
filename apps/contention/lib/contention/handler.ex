defmodule Contention.Handler do
  @behaviour :cowboy_handler

  def init(req, opts) do
    req =
      :cowboy_req.reply(200, %{
        "content-type" => "text/plain"
      }, "Hello world!", req)
    {:ok, req, opts}
  end
end