defmodule Latency.CNode do

  def echo() do
    pid = open()
    echo = &__MODULE__.echo(pid, &1)
    close = fn() -> :ok end
    {echo, close}
  end

  def echo(term) do
    :latency_c_node.call(:echo, [term])
  end

  def echo(pid, term) do
    :latency_c_node.call(pid, :echo, [term], :infinity)
  end

  def open() do
    case :latency_c_node.get_pid() do
      {:ok, pid} ->
        pid
      :error ->
        case :latency_c_node.get_node() do
          {:ok, node} ->
            {:any, node}
          :error ->
            :erlang.error(:notsup)
        end
    end
  end

  def supported?() do
    case :erlang.whereis(:latency_c_node) do
      :undefined -> false
      _ -> true
    end
  end

end
