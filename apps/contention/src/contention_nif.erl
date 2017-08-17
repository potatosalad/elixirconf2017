%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  12 August 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(contention_nif).

-export([spin/1]).
-export([spinsleep/1]).
-export([spinsleep_dirty/1]).
-export([spinsleep_timeslice/1]).
-export([spinsleep_timeslice_dirty/1]).

-on_load(init/0).

spin(_Count) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

spinsleep(_Microseconds) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

spinsleep_dirty(_Microseconds) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

spinsleep_timeslice(_Microseconds) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

spinsleep_timeslice_dirty(_Microseconds) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(contention:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
