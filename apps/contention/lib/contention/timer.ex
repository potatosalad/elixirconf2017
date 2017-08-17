defmodule Contention.Timer do
  use GenServer

  @interval 1000
  @unit :millisecond
  @window 10

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  defmodule State do
    @type t() :: %__MODULE__{
      startup: integer(),
      count: non_neg_integer(),
      drift: non_neg_integer(),
      total: non_neg_integer(),
      timer_start: integer(),
      timer_drift: integer(),
      timer_ref: reference()
    }

    defstruct [
      startup: nil,
      count: 0,
      drift: 0,
      total: 0,
      timer_start: nil,
      timer_drift: 0,
      timer_ref: nil
    ]
  end

  alias __MODULE__.State, as: State

  def init([]) do
    {timer_start, timer_ref} = setup_timer()
    state = %State{
      startup: timer_start,
      timer_start: timer_start,
      timer_ref: timer_ref
    }
    {:ok, state}
  end

  def handle_info({:timeout, timer_ref, :fire}, state = %State{
    startup: startup,
    timer_start: timer_start,
    count: count,
    drift: drift,
    timer_drift: timer_drift,
    timer_ref: timer_ref
  }) when is_reference(timer_ref) do
    timer_stop = :erlang.monotonic_time(@unit)
    timer_real = timer_stop - timer_start
    timer_diff = timer_real - @interval
    timer_late = timer_diff > @window
    timer_drift = timer_drift + timer_diff
      # if timer_late do
      #   timer_drift + @window
      # else
      #   timer_drift + timer_diff
      # end
    count = count + 1
    # total = div(timer_stop - startup - timer_drift + div(@interval, 2), @interval)
    total = div(timer_stop - startup, @interval)
    drift =
      case total - drift do
        ^count ->
          drift
        offset ->
          error = offset - count
          drift + error
      end
    {timer_start, timer_ref} = setup_timer()
    state = %{
      state |
      count: count,
      drift: drift,
      total: total,
      timer_start: timer_start,
      timer_drift: timer_drift,
      timer_ref: timer_ref
    }
    if timer_late do
      require Logger
      Logger.info("#{inspect(state)}")
    end
    # timer_drift = 
    # if timer_late do
    #   require Logger
    #   Logger.info("(#{:erlang.system_info(:scheduler_id)}) timer fired: #{tc} (#{ta}, #{tb})")
    # end
    # {ta, tr} = setup_timer()
    # state = %{ state | ta: ta, tr: tr }
    {:noreply, state}
  end

  @doc false
  defp setup_timer() do
    timer_start = :erlang.monotonic_time(@unit)
    timer_ref = :erlang.start_timer(@interval, :erlang.self(), :fire)
    {timer_start, timer_ref}
  end
end