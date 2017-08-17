%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  14 August 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(contention_event_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init(Pid) ->
	{ok, Pid}.

handle_event(Event, Pid) ->
	catch Pid ! {contention, Event},
	{ok, Pid}.

handle_call(_Request, Pid) ->
	{ok, ok, Pid}.

handle_info({'EXIT', _Parent, shutdown}, _Pid) ->
	remove_handler;
handle_info(_Info, Pid) ->
	{ok, Pid}.

terminate(_Reason, _Pid) ->
	ok.

code_change(_OldVsn, Pid, _Extra) ->
	{ok, Pid}.
