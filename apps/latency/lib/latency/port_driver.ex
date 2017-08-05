defmodule Latency.PortDriver do

  def call_echo(term) do
    :erlang.port_call(:latency_drv, 0, term)
  end

  def control_echo(term) do
    :erlang.binary_to_term(:erlang.port_control(:latency_drv, 0, :erlang.term_to_binary(term)))
  end

  def outputv_echo() do
    port = open()
    echo = &__MODULE__.outputv_echo(port, &1)
    close = fn() ->
      __MODULE__.close(port)
    end
    {echo, close}
  end

  def outputv_echo(port, term) do
    true = :erlang.port_command(port, :erlang.term_to_binary(term))
    receive do
      {^port, {:data, output}} ->
        :erlang.binary_to_term(output)
    end
  end

  def open() do
    :erlang.open_port({:spawn_driver, 'latency_drv'}, [:binary])
  end

  def close(port) do
    :erlang.port_close(port)
  end

end