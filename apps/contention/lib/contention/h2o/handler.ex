defmodule Contention.H2O.Handler do

  def call(_event, _opts) do
    {200, %{
      "content-length" => 12,
      "content-type" => "text/plain"
    }, "Hello world!"}
  end

end