defmodule Latency.Application do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    # Define workers and child supervisors to be supervised
    children = [
      worker(:latency_c_node, []),
      worker(:latency_drv, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Latency.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
