defmodule Contention.SchedulerUsage do
  use GenServer

  @interval 1000

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  defmodule State do
    @type t() :: %__MODULE__{
      interval: integer(),
      prev_sched: {integer(), integer(), integer()},
      timer_ref: reference()
    }

    defstruct [
      interval: nil,
      prev_sched: nil,
      timer_ref: nil
    ]
  end

  alias __MODULE__.State, as: State

  def init([]) do
    _ = :erlang.system_flag(:scheduler_wall_time, true)
    state = start_interval(%State{ interval: @interval, prev_sched: scheduler_wall_time() })
    {:ok, state}
  end

  def handle_info({:timeout, timer_ref, :interval}, state = %State{
    prev_sched: prev_scheduler_wall_time,
    timer_ref: timer_ref
  }) do
    next_scheduler_wall_time = scheduler_wall_time()
    scheduler_usage = scheduler_usage_diff(prev_scheduler_wall_time, next_scheduler_wall_time)
    :ok = :contention_event.notify({:usage, :os.timestamp(), scheduler_usage})
    state = start_interval(%{ state | prev_sched: next_scheduler_wall_time })
    {:noreply, state}
  end

  # Scheduler usage diff originally from https://github.com/ferd/recon
  @doc false
  defp scheduler_usage_diff(prev, next) do
    :lists.map(fn
      ({{i, _a0, t}, {i, _a1, t}}) -> {i, 0.0} # avoid divide by zero
      ({{i, a0, t0}, {i, a1, t1}}) -> {i, (a1 - a0) / (t1 - t0)}
    end, :lists.zip(prev, next))
  end

  @doc false
  defp scheduler_wall_time() do
    :lists.sort(:erlang.statistics(:scheduler_wall_time))
  end

  @doc false
  defp start_interval(state = %State{ interval: interval }) do
    timer_ref = :erlang.start_timer(interval, :erlang.self(), :interval)
    %{ state | timer_ref: timer_ref }
  end
end