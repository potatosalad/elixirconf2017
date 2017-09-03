%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(sverk_tcp_nif).

-export([is_loaded/0]).
-export([connect/3]).
-export([send/2]).
-export([recv/2]).
-export([recv/3]).
-export([setopts/2]).
-export([close/1]).
-export([stats/1]).

-on_load(init/0).

-define(DEBUG_TIMEOUT, (60*1000)).

is_loaded() -> false.

connect(Host, Port, Opts) ->
	case connect_nif(Host, Port, Opts) of
		{ok, Rsrc} ->
			case lists:keyfind(active,1,Opts) of
				{active, false} ->
					{ok, {Rsrc, undefined}};
				{active, once} ->
					ClientPid = self(),
					RecvPid = spawn_link(fun()-> active_loop({Rsrc,self()},ClientPid) end),
					{ok, {Rsrc, RecvPid}}
			end;
		 Err -> Err
	end.

connect_nif(_Host, _Port, _Opts) ->
	erlang:nif_error(not_loaded).

send({Rsrc,_}, Data) ->
	send_do(Rsrc, Data).

send_do(Rsrc, Data) ->
	Ref = make_ref(),
	case send_try_nif(Rsrc, Data, Ref) of
		ok ->
			ok;
		Written when is_integer(Written) ->
			receive
				{select, Rsrc, Ref, ready_output} ->
					<<_:Written/binary, Rest/binary>> = Data,
					send_do(Rsrc, Rest)
			after ?DEBUG_TIMEOUT ->
				exit("send_do ready_output timeout")
			end;
		{error,_}=Err ->
			Err
	end.

send_try_nif(_Rsrc, _Data, _Ref) ->
	erlang:nif_error(not_loaded).
	
recv({Rsrc,undefined}, Length) ->
	recv_do(Rsrc, Length, infinity).

recv({Rsrc,undefined}, Length, Timeout) ->
	recv_do(Rsrc, Length, Timeout).

recv_do(Rsrc, Length, Timeout) ->
	Ref = make_ref(),
	case recv_try_nif(Rsrc, Length, Ref) of
		Bin when is_binary(Bin) ->
			{ok, Bin};
		eagain ->
			receive
				{select, Rsrc, Ref, ready_input} ->
					recv_do(Rsrc, Length, Timeout)
			after Timeout ->
				{error, timeout}
			end;
		{error, _}=Err ->
			Err
	end.

recv_try_nif(_Rsrc, _Length, _Ref) ->
	erlang:nif_error(not_loaded).

setopts({_Rsrc,RecvPid}, [{active,once}]) when is_pid(RecvPid) ->
	RecvPid ! continue.

close({Rsrc,undefined}) ->
	close_nif(Rsrc);
close({_Rsrc,RecvPid}) ->
	RecvPid ! close.

close_nif(_Rsrc) ->
	erlang:nif_error(not_loaded).

active_loop({Rsrc,_}=Sock, Pid) ->
	Ref = make_ref(),
	case recv_try_nif(Rsrc, 0, Ref) of
		Bin when is_binary(Bin) ->
			Pid ! {tcp, Sock, Bin},
			receive
				continue -> active_loop(Sock, Pid);
				close -> close_nif(Rsrc)
			after ?DEBUG_TIMEOUT ->
				exit("active_loop continue timeout")
			end;
		eagain ->
			receive
				{select, Rsrc, Ref, ready_input} ->
					active_loop(Sock, Pid);
				close ->
					close_nif(Rsrc),
					receive
						{select, Rsrc, Ref, ready_input} -> ignore
					after 0 ->
							ignore
					end,
					ok
			after ?DEBUG_TIMEOUT*2 ->
				exit("active_loop ready_input timeout")
			end;
			{error, closed} ->
				Pid ! {tcp_closed, Sock};
		{error, Reason} ->
				Pid ! {tcp_error, Sock, Reason}
	end.


stats({Rsrc,_}) ->
	stats_nif(Rsrc).

stats_nif(_Rsrc) ->
	erlang:nif_error(not_loaded).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(select:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
