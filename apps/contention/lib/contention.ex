defmodule Contention do

  @wait_ms 1000

  def spinsleep(microseconds, count \\ :erlang.system_info(:schedulers_online)) do
    # parent = :erlang.self()
    fn () ->
      _result = :contention_nif.spinsleep(microseconds)
      # :erlang.send(parent, result)
    end
    |> wait(@wait_ms)
    |> spawn_monitors(count)
    |> await()
  end

  def spinsleep_dirty(microseconds, count \\ :erlang.system_info(:schedulers_online)) do
    # parent = :erlang.self()
    fn () ->
      _result = :contention_nif.spinsleep_dirty(microseconds)
      # :erlang.send(parent, result)
    end
    |> wait(@wait_ms)
    |> spawn_monitors(count)
    |> await()
  end

  def spinsleep_timeslice(microseconds, count \\ :erlang.system_info(:schedulers_online)) do
    # parent = :erlang.self()
    fn () ->
      _result = :contention_nif.spinsleep_timeslice(microseconds)
      # :erlang.send(parent, result)
    end
    |> wait(@wait_ms)
    |> spawn_monitors(count)
    |> await()
  end

  def spinsleep_timeslice_dirty(microseconds, count \\ :erlang.system_info(:schedulers_online)) do
    # parent = :erlang.self()
    fn () ->
      _result = :contention_nif.spinsleep_timeslice_dirty(microseconds)
      # :erlang.send(parent, result)
    end
    |> wait(@wait_ms)
    |> spawn_monitors(count)
    |> await()
  end

  # def spinsleep(n, microseconds) when is_integer(n) and n > 0 and is_integer(microseconds) and microseconds >= 0 do
  #   spawn_n(n, fn () ->
  #     :contention_nif.spinsleep(microseconds)
  #   end, [])
  #   # spawn_n_wait(n, fn () ->
  #   #   :contention_nif.spinsleep(microseconds)
  #   # end, wait)
  # end

  # def spinsleep_dirty(n, microseconds) when is_integer(n) and n > 0 and is_integer(microseconds) and microseconds >= 0 do
  #   spawn_n(n, fn () ->
  #     :contention_nif.spinsleep_dirty(microseconds)
  #   end, [])
  #   # spawn_n_wait(n, fn () ->
  #   #   :contention_nif.spinsleep_dirty(microseconds)
  #   # end, wait)
  # end

  def wait(function, milliseconds) do
    fn () ->
      receive do
      after milliseconds ->
        function.()
      end
    end
  end

  def spawn_monitors(function, count) when is_function(function, 0) and is_integer(count) and count > 0 do
    spawn_monitors(function, count, %{})
  end

  @doc false
  defp spawn_monitors(_, 0, monitors) do
    monitors
  end
  defp spawn_monitors(function, count, monitors) do
    {pid, reference} = :erlang.spawn_monitor(function)
    spawn_monitors(function, count - 1, Map.put(monitors, reference, pid))
  end

  @doc false
  defp await(monitors) when map_size(monitors) === 0 do
    :ok
  end
  defp await(monitors) do
    receive do
      {:DOWN, reference, :process, pid, _reason} ->
        case Map.pop(monitors, reference) do
          {^pid, monitors} ->
            await(monitors)
        end
    end
  end

  # @doc false
  # defp spawn_n(0, _, acc) do
  #   :lists.reverse(acc)
  # end
  # defp spawn_n(n, fun, acc) do
  #   pid = :erlang.spawn(fun)
  #   spawn_n(n - 1, fun, [pid | acc])
  # end

  # @doc false
  # defp spawn_n_wait(n, fun, wait) do
  #   ref = :erlang.make_ref()
  #   children = spawn_n(n, fn () ->
  #     receive do
  #       ^ref ->
  #         receive do
  #         after wait ->
  #           fun.()
  #         end
  #     end
  #   end, [])
  #   old_level = :erlang.process_flag(:priority, :high)
  #   for child <- children do
  #     :erlang.send(child, ref)
  #   end
  #   :high = :erlang.process_flag(:priority, old_level)
  #   :ok
  # end

end