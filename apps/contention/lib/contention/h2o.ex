defmodule Contention.H2O do

  def child_spec(ref, port, handler, opts) do
    {
      ref,
      {__MODULE__, :start_link, [ref, port, handler, opts]},
      :permanent, 5000, :worker, [Contention.H2O]
    }
  end

  def start_link(ref, port, handler, opts) when is_integer(port) and port > 0 and port < 65535 and is_atom(handler) do
    :proc_lib.start_link(__MODULE__, :proc_lib_hack, [:erlang.self(), ref, port, handler, opts])
  end

  @doc false
  def proc_lib_hack(parent, ref, port, handler, opts) do
    try do
      init(parent, ref, port, handler, opts)
    catch
      _, :normal -> :erlang.exit(:normal)
      _, :shutdown -> :erlang.exit(:shutdown)
      _, reason = {:shutdown, _} -> :erlang.exit(reason)
      _, reason -> :erlang.exit({reason, :erlang.get_stacktrace()})
    end
  end

  @doc false
  defp init(parent, ref, port, handler, opts) do
    true = :erlang.register(ref, :erlang.self())
    config = [
      {"listen", port},
      {"num-threads", 1},
      {"hosts", [
        {"*", [
          {"paths", [
            {"/", [
              {"erlang.handler", {handler, opts}}
            ]}
          ]}
        ]}
      ]}
    ]
    {:ok, server} = :h2o_server.open()
    {:ok, bindings} = :h2o_server.setcfg(server, config)
    :ok = :h2o_server.start(server)
    [{_, port, _, _}] = bindings
    :ok = :proc_lib.init_ack(parent, {:ok, :erlang.self()})
    response = handler.call(nil, opts)
    loop(parent, ref, server, port, response)
  end

  @doc false
  defp loop(parent, ref, server, port, response) do
    receive do
      {:h2o_port_data, ^port, :ready_input} ->
        handle(:h2o_nif.handler_read(port), parent, ref, server, port, response, [])
      {:system, from, request} ->
        :sys.handle_system_msg(request, from, parent, __MODULE__, [], {parent, ref, server, port, response})
      msg ->
        :error_logger.error_msg('h2o listener ~p received unexpected message ~p~n', [
          ref,
          msg
        ])
    end
  end

  @doc false
  defp handle([event | events], parent, ref, server, port, response={status, headers, body}, acc) do
    handle(events, parent, ref, server, port, response, [{event, status, headers, body} | acc])
  end
  defp handle([], parent, ref, server, port, response, acc) do
    :ok = :h2o_nif.handler_event_reply_batch(acc)
    loop(parent, ref, server, port, response)
  end

  @doc false
  def system_continue(_, _, {parent, ref, server, port, response}) do
    loop(parent, ref, server, port, response)
  end

  @doc false
  def system_terminate(reason, _, _, _state) do
    :erlang.exit(reason)
  end

  @doc false
  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end

end