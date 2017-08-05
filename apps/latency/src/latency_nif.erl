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
-module(latency_nif).

-export([echo/1]).
-export([dirty_cpu_echo/1]).
-export([dirty_io_echo/1]).
-export([future_echo/1]).
-export([thread_new_echo/1]).
-export([thread_queue_echo/1]).
-export([thread_queue_echo/2]).

-on_load(init/0).

echo(_Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

dirty_cpu_echo(_Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

dirty_io_echo(_Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

future_echo(_Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

thread_new_echo(_Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

thread_queue_echo(Term) ->
	thread_queue_echo(Term, 1, 3).

thread_queue_echo(_AsyncRef, _Term) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(latency:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).

%% @private
thread_queue_echo(_Term, Max, Max) ->
	not_found;
thread_queue_echo(Term, Attempt, Max) ->
	R = erlang:make_ref(),
	case thread_queue_echo(R, Term) of
		{ok, {enqueued, PctBusy}} ->
			if
				PctBusy > 0.25 andalso PctBusy =< 1.0 ->
					erlang:bump_reductions(erlang:trunc(2000 * PctBusy));
				true ->
					ok
			end,
			receive
				{R, {error, shutdown}=Error} ->
					%% Work unit was queued, but not executed.
					Error;
				{R, {error, _Reason}=Error} ->
					%% Work unit returned an error.
					Error;
				{R, Reply} ->
					Reply
			end;
		{error, eagain} ->
			thread_queue_echo(Term, Attempt + 1, Max);
		Other ->
			Other
	end.
