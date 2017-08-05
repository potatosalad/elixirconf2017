defmodule Latency do

  def compare(term, iterations) do
    functions = [
      nif_echo: &Latency.NIF.echo/1,
      nif_dirty_cpu_echo: &Latency.NIF.dirty_cpu_echo/1,
      nif_dirty_io_echo: &Latency.NIF.dirty_io_echo/1,
      nif_future_echo: &Latency.NIF.future_echo/1,
      nif_thread_new_echo: &Latency.NIF.thread_new_echo/1,
      nif_thread_queue_echo: &Latency.NIF.thread_queue_echo/1,
      port_echo: &Latency.Port.echo/0,
      port_driver_call_echo: &Latency.PortDriver.call_echo/1,
      port_driver_control_echo: &Latency.PortDriver.control_echo/1,
      port_driver_outputv_echo: &Latency.PortDriver.outputv_echo/0
    ]
    functions =
      if Latency.CNode.supported?() do
        [
          {:cnode_echo_0, &Latency.CNode.echo/0}#,
          # {:cnode_echo_1, &Latency.CNode.echo/1}
          | functions
        ]
      else
        functions
      end
    compare(functions, term, iterations)
  end

  def compare(functions, term, iterations) when is_list(functions) do
    for {label, function} <- functions, into: [] do
      {label, measure(label, function, term, iterations), function}
    end
  end

  def measure(label, function, term, iterations) when (is_function(function, 0) or is_function(function, 1)) and is_integer(iterations) and iterations >= 1 do
    Task.async(fn() ->
      {:ok, ref} = :hdr_histogram.open(:erlang.convert_time_unit(5, :second, :nanosecond), 1)
      {function, close} = maybe_open(function)
      {min_nsec, max_nsec, acc_nsec, min_reds, max_reds, acc_reds} =
        bench_loop(iterations, ref, 0, 0, 0, 0, 0, 0, function, term, :erlang.self())
      :ok = maybe_close(close)
      # :hdr_histogram.print(ref, :csv)
      :hdr_histogram.log(ref, :classic, :binary.bin_to_list("hist-#{label}.hgrm"))
      # IO.puts "Min #{:hdr_histogram.min(ref)}"
      # IO.puts "Mean #{:hdr_histogram.mean(ref)}"
      # IO.puts "Median #{:hdr_histogram.median(ref)}"
      # IO.puts "Max #{:hdr_histogram.max(ref)}"
      # IO.puts "Stddev #{:hdr_histogram.stddev(ref)}"
      # IO.puts "99ile #{:hdr_histogram.percentile(ref,99.0)}"
      # IO.puts "99.9999ile #{:hdr_histogram.percentile(ref,99.9999)}"
      # IO.puts "Memory Size #{:hdr_histogram.get_memory_size(ref)}"
      # IO.puts "Total Count #{:hdr_histogram.get_total_count(ref)}"
      :hdr_histogram.close(ref)
      # Latency.compare(0, 10000)
      min_nsec = :erlang.convert_time_unit(min_nsec, :native, :microsecond)
      max_nsec = :erlang.convert_time_unit(max_nsec, :native, :microsecond)
      acc_nsec = :erlang.convert_time_unit(acc_nsec, :native, :microsecond)
      acc_nsec = acc_nsec / iterations
      acc_reds = acc_reds / iterations
      {{min_nsec, max_nsec, acc_nsec}, {min_reds, max_reds, acc_reds}}
    end)
    |> Task.await(:infinity)
  end

  @doc false
  defp bench_loop(0, _ref, min_nsec, max_nsec, acc_nsec, min_reds, max_reds, acc_reds, _function, _term, _pid) do
    {min_nsec, max_nsec, acc_nsec, min_reds, max_reds, acc_reds}
  end
  defp bench_loop(i, ref, min_nsec, max_nsec, acc_nsec, min_reds, max_reds, acc_reds, function, term, pid) do
    ta = :erlang.monotonic_time()
    # {:reductions, ra} = :erlang.process_info(pid, :reductions)
    # {ra, _} = :erlang.statistics(:exact_reductions)
    ^term = function.(term)
    # {:reductions, rb} = :erlang.process_info(pid, :reductions)
    # {rb, _} = :erlang.statistics(:exact_reductions)
    tb = :erlang.monotonic_time()
    ra = 0
    rb = 0
    r = rb - ra
    t = tb - ta
    :ok = :hdr_histogram.record(ref, t)
    min_nsec =
      if min_nsec == 0 do
        t
      else
        min(t, min_nsec)
      end
    max_nsec = max(t, max_nsec)
    acc_nsec = acc_nsec + t
    min_reds =
      if min_reds == 0 do
        r
      else
        min(r, min_reds)
      end
    max_reds = max(r, max_reds)
    acc_reds = acc_reds + r
    bench_loop(i - 1, ref, min_nsec, max_nsec, acc_nsec, min_reds, max_reds, acc_reds, function, term, pid)
  end

  @doc false
  defp maybe_open(function) when is_function(function, 0) do
    function.()
  end
  defp maybe_open(function) do
    {function, nil}
  end

  @doc false
  defp maybe_close(nil) do
    :ok
  end
  defp maybe_close(function) when is_function(function, 0) do
    _ = function.()
    :ok
  end

end
