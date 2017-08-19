defmodule Contention.SchedulerUtilization do
  use GenServer

  @interval 250

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def fetch() do
    GenServer.call(__MODULE__, :fetch)
  end

  defmodule State do
    @type t() :: %__MODULE__{
      interval: integer(),
      prev_swt: {integer(), integer(), integer()},
      timer_ref: reference()
    }

    defstruct [
      interval: nil,
      prev_swt: nil,
      timer_ref: nil
    ]
  end

  alias __MODULE__.State, as: State

  def init([]) do
    :foo = :ets.new(:foo, [:named_table, :public, :set])
    _ = :erlang.system_flag(:scheduler_wall_time, true)
    state = start_interval(%State{ interval: @interval, prev_swt: scheduler_wall_time() })
    {:ok, state}
  end

  def handle_call(:fetch, _from, state) do
    {:ok, data, state} = update(state)
    {:reply, {:erlang.system_time(:nanosecond), data}, state}
  end

  def handle_info({:timeout, timer_ref, :interval}, state = %State{ timer_ref: timer_ref }) do
    {:ok, data, state} = update(%{ state | timer_ref: nil })
    :ok = :contention_event.notify({:wall_time, :erlang.system_time(:nanosecond), data})
    state = start_interval(state)
    {:noreply, state}
  end

  @doc false
  defp scheduler_utilization(swt0, swt1) do
    scheduler_utilization(swt0, swt1, :erlang.system_info(:schedulers), 0.0, 0.0)
  end

  @doc false
  defp scheduler_utilization([], [], schedulers, n, d) do
    dirty_cpu_schedulers = :erlang.system_info(:dirty_cpu_schedulers)
    normal_utilization = n / schedulers
    dirty_utilization = d / dirty_cpu_schedulers
    {normal_utilization, dirty_utilization}
  end
  defp scheduler_utilization([{i, a0, t0} | swt0], [{i, a1, t1} | swt1], schedulers, n, d) do
    utilization = scheduler_utilization_calc((a1 - a0), (t1 - t0))
    if i > schedulers do
      scheduler_utilization(swt0, swt1, schedulers, n, d + utilization)
    else
      scheduler_utilization(swt0, swt1, schedulers, n + utilization, d)
    end
  end

  @doc false
  defp scheduler_utilization_calc(adelta, tdelta) when tdelta > 0 do
    adelta / tdelta
  end
  defp scheduler_utilization_calc(_, _) do
    0.0
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

  @doc false
  defp update(state = %State{ prev_swt: prev_swt }) do
    next_swt = scheduler_wall_time()
    {:ok, scheduler_utilization(prev_swt, next_swt), %{ state | prev_swt: next_swt }}
  end
end