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
-module(contention_event).

-define(MANAGER, contention_event_manager).

%% API
-export([manager/0]).
-export([add_handler/2]).
-export([delete_handler/2]).
-export([notify/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(manager(), Handler, Pid).

delete_handler(Handler, Pid) ->
	gen_event:delete_handler(manager(), Handler, Pid).

notify(Message) ->
	gen_event:notify(manager(), Message).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
