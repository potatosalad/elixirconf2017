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
      cowboy_server(),
    ]
    children = h2o_server(children)

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
  defp cowboy_server() do
    dispatch = :cowboy_router.compile([{:_, [{:_, Contention.Handler, []}]}])
    ref = Contention.Handler.HTTP
    transport_options = [
      port: 29593
    ]
    protocol_options = %{
      env: %{ dispatch: dispatch }#,
      # stream_handlers: [Contention.StreamHandler]
    }
    {
      {:ranch_listener_sup, ref},
      {:cowboy, :start_clear, [
        ref, transport_options, protocol_options
      ]},
      :permanent, :infinity, :supervisor, [:ranch_listener_sup]
    }
  end

  @doc false
  defp h2o_server(child_specs) do
    if Code.ensure_loaded?(:h2o) do
      child_specs ++ [Contention.H2O.child_spec(Contention.H2O, 29594, Contention.H2O.Handler, [])]
    else
      child_specs
    end
  end
end
