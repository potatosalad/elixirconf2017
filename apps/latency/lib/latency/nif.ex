defmodule Latency.NIF do

  defdelegate echo(term), to: :latency_nif
  defdelegate dirty_cpu_echo(term), to: :latency_nif
  defdelegate dirty_io_echo(term), to: :latency_nif
  defdelegate future_echo(term), to: :latency_nif

  def thread_new_echo(term) do
    tag = :latency_nif.thread_new_echo(term)
    receive do
      {^tag, msg} ->
        msg
    end
  end

  defdelegate thread_queue_echo(term), to: :latency_nif

end
