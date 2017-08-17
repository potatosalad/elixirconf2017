defmodule Contention.Sink do
  # @behaviour :vmstats_sink

  require Logger

  # def collect(:gauge, key, value) do
  #   key = :erlang.iolist_to_binary(key)
  #   # :folsom_metrics.new_gauge(key)
  #   :folsom_metrics.notify(key, value, :gauge)
  #   # require Logger
  #   # Logger.info("#{key} = #{value}")
  #   # folsom_metrics:new_gauge(Name).
  #   :ok
  #   # Logger.info(inspect({type, :erlang.iolist_to_binary(key), value }))
  # end
  # def collect(:counter, key, value) do
  #   key = :erlang.iolist_to_binary(key)
  #   # :folsom_metrics.notify(key, {:inc, value}, :counter)
  #   :folsom_metrics.notify(key, value, :spiral)
  #   # Logger.info(inspect({type, :erlang.iolist_to_binary(key), value }))
  #   :ok
  # end
  # def collect(:timing, key, value) do
  #   # Logger.info(inspect({ :timing, :erlang.iolist_to_binary(key), value }))
  #   key = :erlang.iolist_to_binary(key)
  #   # :folsom_metrics.notify(key, value, :histogram)
  #   case key do
  #     << "vmstats.scheduler_wall_time.", n, ".total" >> when n in ?0..?8 ->
  #       :folsom_metrics.notify(key, value, :meter_reader)
  #     # << "vmstats.scheduler_wall_time.1.total" >> ->
  #       # :folsom_metrics.notify(key, value, :meter_reader)
  #     _ ->
  #       :folsom_metrics.notify(key, value, :histogram)
  #   end
  #   :ok
  # end
  # def collect(:timing, key, value) do
  #   key = :erlang.iolist_to_binary(key)
  #   case :binary.split(key, << ?. >>, [:global, :trim_all]) do
  #     ["vmstats", "scheduler_wall_time", scheduler_id, type] when type in ["active", "total"] ->
  #       scheduler_id = :erlang.binary_to_integer(scheduler_id)
  #       :contention_event.notify({scheduler_id, value})
  #     _ ->
  #       :ok
  #   end
  # end
  def collect(_, _, _) do
    :ok
  end

end