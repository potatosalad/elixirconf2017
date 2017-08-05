defmodule Latency.Port do

  def echo() do
    port = open()
    echo = &__MODULE__.echo(port, &1)
    close = fn() ->
      __MODULE__.close(port)
    end
    {echo, close}
  end

  def echo(port, term) do
    true = :erlang.port_command(port, :erlang.term_to_binary(term))
    receive do
      {^port, {:data, output}} ->
        try do
          :erlang.binary_to_term(output)
        catch _,_ ->
          receive_data(port, output)
        end
    end
  end


  # def echo(port, term) do
  #   input = :erlang.term_to_binary(term)
  #   size = byte_size(input)
  #   true = :erlang.port_command(port, [<< size :: unsigned-big-integer-unit(1)-size(32) >>, input])
  #   receive do
  #     {^port, {:data, << ^size :: unsigned-big-integer-unit(1)-size(32), output :: binary-size(size) >>}} ->
  #       :erlang.binary_to_term(output)
  #     {^port, {:data, << ^size :: unsigned-big-integer-unit(1)-size(32), so_far :: binary >>}} ->
  #       receive_data(port, size - byte_size(so_far), so_far)
  #   end
  # end

  def open() do
    # :erlang.open_port({:spawn_executable, :filename.join([:latency.priv_dir(), 'latency_port'])}, [:binary, {:packet, 4}])
    :erlang.open_port({:spawn_executable, :filename.join([:latency.priv_dir(), 'latency_port'])}, [:binary, :stream])
  end

  def close(port) do
    :erlang.port_close(port)
  end

  defp receive_data(port, so_far) do
    receive do
      {^port, {:data, rest}} ->
        output = << so_far :: binary, rest :: binary >>
        try do
          :erlang.binary_to_term(output)
        catch _,_ ->
          receive_data(port, output)
        end
    after
      100 ->
        {:error, :timeout}
    end
  end

  # defp receive_data(port, left, so_far) do
  #   receive do
  #     {^port, {:data, << rest :: binary-size(left) >>}} ->
  #       :erlang.binary_to_term(:erlang.iolist_to_binary([so_far, rest]))
  #     {^port, {:data, rest}} ->
  #       receive_data(port, left - byte_size(rest), [so_far, rest])
  #   end
  # end

end