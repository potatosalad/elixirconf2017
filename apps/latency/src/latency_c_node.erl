%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  06 June 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(latency_c_node).

-behaviour(gen_statem).

%% Public API
-export([start_link/0]).
-export([call/2]).
-export([call/3]).
-export([call/4]).
-export([get_node/0]).
-export([get_pid/0]).
%% gen_statem callbacks
-export([callback_mode/0]).
-export([init/1]).
-export([terminate/3]).
-export([code_change/4]).
%% gen_statem states
-export([unregistered/3]).
-export([unregistered_busy/3]).
-export([registered/3]).
-export([registered_busy/3]).

%% Private API
-export([load/0]).

%% Records
-record(data, {
	node = undefined :: undefined | node(),
	pid  = undefined :: undefined | pid(),
	port = undefined :: undefined | port(),
	tag  = undefined :: undefined | reference()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link()
	-> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Function, Arguments) ->
	call(Function, Arguments, infinity).

call(Function, Arguments, Timeout) ->
	case get_pid() of
		{ok, Pid} ->
			call(Pid, Function, Arguments, Timeout);
		error ->
			case get_node() of
				{ok, Node} ->
					call({any, Node}, Function, Arguments, Timeout);
				error ->
					erlang:error(notsup)
			end
	end.

call(To, Function, Arguments, Timeout) ->
	Tag = erlang:make_ref(),
	_ = erlang:send(To, {call, {erlang:self(), Tag}, Function, Arguments}),
	receive
		{Tag, Reply} ->
			Reply
	after
		Timeout ->
			{error, timeout}
	end.

get_node() ->
	case ets:lookup(?MODULE, node) of
		[] ->
			error;
		[{node, Node}] ->
			{ok, Node}
	end.

get_pid() ->
	case ets:lookup(?MODULE, pid) of
		[] ->
			error;
		[{pid, Pid}] ->
			{ok, Pid}
	end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
	state_functions.

init([]) ->
	erlang:process_flag(trap_exit, true),
	?MODULE = ets:new(?MODULE, [
		named_table,
		public,
		ordered_set,
		{read_concurrency, true}
	]),
	case load() of
		{ok, Node, Port} ->
			true = ets:insert(?MODULE, {node, Node}),
			Data = #data{node=Node, port=Port},
			{ok, unregistered, Data, [{state_timeout, 0, request_start}]};
		error ->
			ignore
	end.

terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%%===================================================================
%%% gen_statem states
%%%===================================================================

unregistered(state_timeout, request_start, Data) ->
	{next_state, unregistered_busy, request_start(Data), [{state_timeout, timer:seconds(15), request_timeout}]};
unregistered(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

unregistered_busy(state_timeout, request_timeout, Data) ->
	{next_state, unregistered, request_timeout(Data), [{state_timeout, timer:seconds(15) + timer:seconds(rand:uniform(5)), request_start}]};
unregistered_busy(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

registered(state_timeout, request_start, Data) ->
	{next_state, registered_busy, request_start(Data), [{state_timeout, timer:seconds(15), request_timeout}]};
registered(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

registered_busy(state_timeout, request_timeout, Data) ->
	{next_state, registered, request_timeout(Data), [{state_timeout, timer:seconds(15) + timer:seconds(rand:uniform(5)), request_start}]};
registered_busy(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
maybe_register(Node, Pid) ->
	true = ets:insert(?MODULE, {node, Node}),
	true = ets:insert(?MODULE, {pid, Pid}),
	true.

%% @private
handle_event(info, {Tag, Pid}, Data0=#data{node=Node, tag=Tag}) ->
	true = maybe_register(Node, Pid),
	Data = Data0#data{pid=Pid, tag=undefined},
	{next_state, registered, Data, [{state_timeout, timer:minutes(5) + timer:minutes(rand:uniform(5)), request_start}]}.

%% @private
request_start(Data0=#data{node=Node}) ->
	Tag = erlang:make_ref(),
	_ = erlang:send({any, Node}, {call, {erlang:self(), Tag}, self, []}),
	Data = Data0#data{tag=Tag},
	Data.

%% @private
request_timeout(Data0=#data{}) ->
	Data = Data0#data{tag=undefined},
	Data.

%%%===================================================================
%%% Private API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Load C node
%% @spec load() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
load() ->
	case erlang:get_cookie() of
		nocookie ->
			error;
		CookieAtom ->
			Cookie = erlang:atom_to_binary(CookieAtom, unicode),
			Command = filename:join([latency:priv_dir(), "latency_c_node"]),
			{Type, Nodename} = c_node_name(),
			Arguments = [
				<<"-c">>, Cookie,
				Type, erlang:atom_to_binary(Nodename, unicode)
			],
			Port = erlang:open_port({spawn_executable, Command}, [{args, Arguments}, binary]),
			{ok, Nodename, Port}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
c_node_name() ->
	Localname = erlang:atom_to_binary(erlang:node(), unicode),
	[Alivename, Hostname] = binary:split(Localname, << $@ >>),
	Nodename = erlang:binary_to_atom(<< Alivename/binary, "cnode1@", Hostname/binary >>, unicode),
	case binary:match(Hostname, << $. >>) of
		nomatch ->
			{<<"-s">>, Nodename};
		_ ->
			{<<"-n">>, Nodename}
	end.
