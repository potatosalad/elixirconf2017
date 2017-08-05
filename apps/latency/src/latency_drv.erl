%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  01 June 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(latency_drv).

-behaviour(gen_server).

%% Public API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Private API
-export([load/0]).
-export([unload/0]).

%% Records
-record(state, {
	port = undefined :: undefined | port()
}).

%% Macros
-define(SERVER, latency_drv_server).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link()
	-> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([])
	-> ignore | {ok, #state{}} | {stop, any()}.
init([]) ->
	erlang:process_flag(trap_exit, true),
	case load() of
		ok ->
			Port = erlang:open_port({spawn_driver, "latency_drv"}, [binary]),
			true = erlang:register(?MODULE, Port),
			State = #state{port=Port},
			{ok, State};
		{error, LoadError} ->
			LoadErrorStr = erl_ddll:format_error(LoadError),
			ErrorStr = erlang:iolist_to_binary(io_lib:format(
				"could not load driver ~s: ~p",
				["latency_drv", LoadErrorStr])),
			{stop, ErrorStr}
	end.

-spec handle_call(any(), {pid(), any()}, #state{})
	-> {reply, any(), #state{}}.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

-spec handle_cast(any(), #state{})
	-> {noreply, #state{}} | {stop, any(), #state{}}.
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), #state{})
	-> {noreply, #state{}}.
handle_info(_Info, State) ->
	error_logger:warning_msg("Unhandled info: ~p~n", [_Info]),
	{noreply, State}.

-spec terminate(any(), #state{})
	-> ok.
terminate(_Reason, #state{port=Port}) ->
	true = erlang:unregister(?MODULE),
	true = erlang:port_close(Port),
	ok.

-spec code_change(any(), #state{}, any())
	-> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Private API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Load port driver
%% @spec load() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
load() ->
	{ok, Drivers} = erl_ddll:loaded_drivers(),
	case lists:member("latency_drv", Drivers) of
		true ->
			ok;
		false ->
			case erl_ddll:load(latency:priv_dir(), "latency_drv") of
				ok ->
					ok;
				{error, already_loaded} ->
					ok;
				{error, Error} ->
					error_logger:error_msg(
						?MODULE_STRING ": Error loading ~p: ~p~n",
						["latency_drv", erl_ddll:format_error(Error)]
					),
					{error, Error}
			end
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc Unload port driver
%% @spec unload() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unload() ->
	case erl_ddll:unload_driver("latency_drv") of
		ok ->
			ok;
		{error, Error} ->
			error_logger:error_msg(
				?MODULE_STRING ": Error unloading ~p: ~p~n",
				["latency_drv", erl_ddll:format_error(Error)]
			),
			{error, Error}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
