defmodule Contention.StreamHandler do
  @behaviour :cowboy_stream

  def init(_streamid, _req, _opts) do
    commands = [
      {:headers, 200, %{"content-type" => "text/plain"}},
      {:data, :fin, "Hello world!"},
      :stop
    ]
    {commands, nil}
  end

  def data(_streamid, _fin, _data, state) do
    {[], state}
  end

  def info(_streamid, _message, state) do
    {[], state}
  end

  def terminate(_streamid, _reason, _state) do
    :ok
  end

  def early_error(_streamid, _reason, _partial_req, resp, _opts) do
    resp
  end
end