defmodule Contention.Application do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      # worker(Contention.Timer, []),
      # worker(Contention.SchedulerUsage, []),
      worker(Contention.SchedulerUtilization, []),
      event_manager(),
      http_server(),
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Contention.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc false
  defp event_manager() do
    local_name = :contention_event.manager()
    {
      local_name,
      {:gen_event, :start_link, [{:local, local_name}]},
      :permanent, 5000, :worker, [:gen_event]
    }
  end

  @doc false
  defp http_server() do
    dispatch = :cowboy_router.compile([{:_, [{:_, Contention.Handler, []}]}])
    ref = Contention.Handler.HTTP
    transport_options = [
      port: 29593
    ]
    protocol_options = %{
      env: %{ dispatch: dispatch }
    }
    {
      {:ranch_listener_sup, ref},
      {:cowboy, :start_clear, [
        ref, transport_options, protocol_options
      ]},
      :permanent, :infinity, :supervisor, [:ranch_listener_sup]
    }
  end
end
