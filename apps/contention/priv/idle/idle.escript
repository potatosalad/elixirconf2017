#!/usr/bin/env escript
%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(idle).
-export([main/1]).

main([Node, Cookie, Filename]) ->
	{ok, _} = net_kernel:start([erlang:list_to_atom(Node), longnames]),
	true = erlang:set_cookie(erlang:node(), erlang:list_to_atom(Cookie)),
	Parent = erlang:self(),
	Worker = erlang:spawn(fun() -> init(Parent, Filename) end),
	true = erlang:register(worker, Worker),
	io:format("erl: started~n"),
	receive
		{stopped, Worker} ->
			io:format("erl: stopped~n")
	after
		infinity ->
			ok
	end;
main(_) ->
	io:format("usage: idle.escript NODE COOKIE FILENAME\n"),
	erlang:halt(1).

connect(Node) ->
	true = net_kernel:connect_node(Node),
	ok = rpc:call(Node, contention_event, add_handler, [contention_event_handler, erlang:self()]),
	ok.

disconnect(Node) ->
	ok = rpc:call(Node, contention_event, delete_handler, [contention_event_handler, erlang:self()]),
	ok.

-record(state, {
	parent = nil :: pid(),
	node = nil :: node(),
	filename = nil :: string(),
	created = false :: boolean(),
	idle = 0 :: integer(),
	wait = 10 :: integer(),
	startup = nil :: integer(),
	prev_ts = nil :: integer(),
	prev_da = nil :: term(),
	test = nil :: binary(),
	test_ref = nil :: reference(),
	done = true :: boolean(),
	plan = [] :: [binary()],
	tests = #{} :: #{ binary() => {module(), atom(), [term()]} }
}).

-define(test_func, spinsleep_timeslice_dirty).

init(Parent, Filename) ->
	State = #state{
		parent = Parent,
		node = 'contention@127.0.0.1',
		filename = Filename,
		plan = [
			<<"1x100s">>,
			<<"10x100s">>,
			<<"100x100s">>,
			<<"1000x100s">>,
			<<"10000x100s">>
		],
		tests = #{
			<<"1x100s">> => {'Elixir.Contention', ?test_func, [1000 * 100000, erlang:system_info(schedulers) * 1]},
			<<"10x100s">> => {'Elixir.Contention', ?test_func, [1000 * 100000, erlang:system_info(schedulers) * 10]},
			<<"100x100s">> => {'Elixir.Contention', ?test_func, [1000 * 100000, erlang:system_info(schedulers) * 100]},
			<<"1000x100s">> => {'Elixir.Contention', ?test_func, [1000 * 100000, erlang:system_info(schedulers) * 1000]},
			<<"10000x100s">> => {'Elixir.Contention', ?test_func, [1000 * 100000, erlang:system_info(schedulers) * 10000]}
		}
	},
	ok = connect(State#state.node),
	loop(State).

loop(State=#state{test_ref=TestRef}) ->
	receive
		{contention, {usage, OSTimestamp, Data}} ->
			Timestamp = unixtime(OSTimestamp),
			handle(State, Timestamp, Data);
		{TestRef, Result} ->
			io:format("Result: ~p~n", [Result]),
			loop(State#state{done=true, test_ref=nil});
		stop ->
			ok = disconnect(State#state.node),
			ok = terminate(State),
			State#state.parent ! {stopped, erlang:self()},
			ok
	end.

handle(State=#state{created=false}, Timestamp, Data) ->
	true = create(State, Timestamp),
	handle(State#state{created=true, startup=Timestamp, prev_ts=Timestamp, prev_da=Data}, Timestamp, Data);
handle(State0=#state{prev_ts=T0, prev_da=D0}, T1, D1) ->
	Events0 = [],
	{State1, Events1} =
		case T1 - T0 of
			D when D > 2 ->
				make_errors(State0, lists:seq(1, D - 1), T0, Events0);
			D when D > 1 ->
				make_points(State0, lists:seq(1, D - 1), T0, D0, Events0);
			_ ->
				{State0, Events0}
		end,
	{State2, Event0} = make_point(State1, T1, D1),
	State3 = maybe_update_idle(State2#state{prev_ts=T1, prev_da=D1}),
	State4 = maybe_stop_test(State3),
	{State5, Event1} = maybe_start_test(State4, Event0),
	Events2 = lists:reverse([Event1 | Events1]),
	ok = notify_all(State5, Events2),
	loop(State5).

terminate(State=#state{filename=Filename, startup=Start, prev_ts=End}) when is_integer(End) ->
	Current = unixtime(),
	StartOfDay = (Current - (Current rem 86400)) + 21600, % convert to midnight MDT
	Shift = StartOfDay - Start,
	Seconds = End - Start,
	Command = io_lib:format(
		"rrdtool graph ~s.svg \\~n"
		"--width 600 \\~n"
		"--height 200 \\~n"
		"--start 00:00 \\~n"
		"--end start+~wseconds \\~n"
		"--title '~s' \\~n"
		"--vertical-label 'scheduler usage' \\~n"
		"--imgformat SVG \\~n"
		"--border 0 \\~n"
		"--font DEFAULT:0:Consolas \\~n"
		"--upper-limit 8 \\~n"
		"--lower-limit -8 \\~n"
		"--rigid \\~n"
		"~s \\~n"
		"~s \\~n"
		"'CDEF:normal=normal0,100,/' \\~n"
		"'CDEF:dirty=dirty0,100,/,-1,*' \\~n"
		"'CDEF:ln1=normal,normal,UNKN,IF' \\~n"
		"'CDEF:ln2=dirty,dirty,UNKN,IF' \\~n"
		"~s \\~n"
		"'TICK:error#e60073a0:1:  Error' \\~n"
		"'AREA:normal#48c4ec: Normal' \\~n"
		"'AREA:dirty#54ec48: Dirty' \\~n"
		"'LINE1:ln1#1598c3' \\~n"
		"'LINE1:ln2#24bc14' \\~n"
		"'HRULE:0#000000:dashes=3,5:dash-offset=5' ~n"
		, [
			Filename,
			Seconds,
			Filename,
			graph_def(State),
			graph_shift(State, Shift),
			graph_tick(State)
		]
	),
	file:write_file(Filename ++ ".txt", Command),
	_Result = os:cmd(Command),
	% io:format("~s~n~n~s~n", [Command, Result]),
	ok;
terminate(_) ->
	ok.

graph_def(State=#state{tests=Tests0}) ->
	Tests1 = lists:reverse(lists:sort(maps:keys(Tests0))),
	Tests2 = [<<"normal">>, <<"dirty">>, <<"error">> | Tests1],
	graph_def(State, Tests2, []).

graph_def(_, [], []) ->
	<<>>;
graph_def(_, [], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
graph_def(State=#state{filename=Filename, startup=Start, prev_ts=End}, [Key0 | Keys], Acc0) ->
	Key =
		case Key0 of
			<<"normal">> ->
				<<"normal0">>;
			<<"dirty">> ->
				<<"dirty0">>;
			_ ->
				Key0
		end,
	Value = io_lib:format("'DEF:~s=~s.rrd:~s:MAX:start=~w:end=~w:step=1'", [
		Key,
		Filename,
		Key0,
		Start,
		End
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	graph_def(State, Keys, Acc1).

graph_shift(State=#state{tests=Tests0}, Shift) ->
	Tests1 = lists:reverse(lists:sort(maps:keys(Tests0))),
	Tests2 = [<<"normal">>, <<"dirty">>, <<"error">> | Tests1],
	graph_shift(State, Tests2, Shift, []).

graph_shift(_, [], _, []) ->
	<<>>;
graph_shift(_, [], _, [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
graph_shift(State, [Key0 | Keys], Shift, Acc0) ->
	Key =
		case Key0 of
			<<"normal">> ->
				<<"normal0">>;
			<<"dirty">> ->
				<<"dirty0">>;
			_ ->
				Key0
		end,
	Value = io_lib:format("'SHIFT:~s:~w'", [
		Key,
		Shift
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	graph_shift(State, Keys, Shift, Acc1).

graph_tick(State=#state{tests=Tests0}) ->
	Tests1 = lists:reverse(lists:sort(maps:keys(Tests0))),
	graph_tick(State, Tests1, []).

graph_tick(_, [], []) ->
	<<>>;
graph_tick(_, [], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
graph_tick(State, [Key0 | Keys], Acc0) ->
	Key =
		case binary:split(Key0, <<"x">>) of
			[K, _] ->
				<< K/binary, $x >>;
			_ ->
				Key0
		end,
	Value = io_lib:format("'TICK:~s#00000020:1:  ~s'", [
		Key0,
		Key
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	graph_tick(State, Keys, Acc1).

is_idle([]) ->
	true;
is_idle([{_, V} | Rest]) when V < 10 ->
	is_idle(Rest);
is_idle(_) ->
	false.

make_errors(State, [], _, Events) ->
	{State, Events};
make_errors(State0, [I | Is], Timestamp, Events) ->
	Event0 = {Timestamp + I, #{ <<"error">> => 1 }},
	{State1, Event1} = maybe_record_test(State0, Event0),
	make_errors(State1, Is, Timestamp, [Event1 | Events]).

make_event(Timestamp, Data) ->
	make_event(Timestamp, Data, #{ <<"dirty">> => 0, <<"normal">> => 0 }).

make_event(Timestamp, [], Acc) ->
	{Timestamp, Acc};
make_event(Timestamp, [{I, V0} | Rest], Acc=#{ <<"dirty">> := X0 }) when I >= 9 ->
	V1 = erlang:round(V0 * 100),
	X1 = X0 + V1,
	make_event(Timestamp, Rest, Acc#{ <<"dirty">> := X1 });
make_event(Timestamp, [{_I, V0} | Rest], Acc=#{ <<"normal">> := X0 }) ->
	V1 = erlang:round(V0 * 100),
	X1 = X0 + V1,
	make_event(Timestamp, Rest, Acc#{ <<"normal">> := X1 }).

make_point(State0, Timestamp, Data) ->
	Event0 = make_event(Timestamp, Data),
	{State1, Event1} = maybe_record_test(State0, Event0),
	{State1, Event1}.

make_points(State, [], _, _, Events) ->
	{State, Events};
make_points(State0, [I | Is], Timestamp, Data, Events) ->
	{State1, Event} = make_point(State0, Timestamp + I, Data),
	make_points(State1, Is, Timestamp, Data, [Event | Events]).

maybe_record_test(State=#state{test=nil}, Event) ->
	{State, Event};
maybe_record_test(State=#state{test=Test}, {Timestamp, Event0}) ->
	Event1 = maps:put(Test, 1, Event0),
	{State, {Timestamp, Event1}}.

maybe_start_test(State0=#state{node=Node, idle=Idle, done=true, wait=0, test=nil, plan=[Test | Rest], tests=Tests}, {Timestamp, Event0}) when Idle > 10 ->
	TestRef = erlang:make_ref(),
	Self = erlang:self(),
	{M, F, A} = maps:get(Test, Tests),
	_ = erlang:spawn(fun() ->
		Result = rpc:block_call(Node, M, F, A),
		Self ! {TestRef, Result},
		erlang:exit(normal)
	end),
	Event1 =
		case maps:is_key(Test, Event0) of
			true ->
				Event0;
			false ->
				maps:put(Test, 1, Event0)
		end,
	State1 = State0#state{done=false, test=Test, plan=Rest, test_ref=TestRef},
	{State1, {Timestamp, Event1}};
maybe_start_test(State=#state{idle=Idle, done=true, wait=0, test=nil, plan=[]}, Event) when Idle > 10 ->
	_ = erlang:send(erlang:self(), stop),
	{State, Event};
maybe_start_test(State=#state{wait=Wait}, Event) when Wait > 0 ->
	{State#state{wait=(Wait - 1)}, Event};
maybe_start_test(State, Event) ->
	{State, Event}.

maybe_stop_test(State=#state{test=nil}) ->
	State;
maybe_stop_test(State=#state{done=true}) ->
	State#state{idle=0, test=nil};
maybe_stop_test(State) ->
	State.

maybe_update_idle(State=#state{idle=Idle, prev_da=Data}) ->
	case is_idle(Data) of
		true ->
			State#state{idle=(Idle + 1)};
		false ->
			State#state{idle=0}
	end.

notify_all(_, []) ->
	ok;
notify_all(State, [Event | Events]) ->
	ok = notify(State, Event),
	notify_all(State, Events).

notify(#state{filename=Filename}, {Timestamp, Event0}) ->
	Event = lists:sort(maps:to_list(Event0)),
	Template = template(Event),
	Values = values(Event),
	Command = io_lib:format("rrdtool update \"~s.rrd\" -t ~s ~w:~s", [
		Filename,
		Template,
		Timestamp,
		Values
	]),
	Result = os:cmd(Command),
	io:format("~s~n~n~s~n", [Command, Result]),
	ok.

create(#state{filename=Filename, plan=Plan}, Timestamp) ->
	Command = io_lib:format("rrdtool create \"~s.rrd\" --start ~w --step 1 ~s RRA:AVERAGE:0.5:1:6000", [
		Filename,
		Timestamp - 1,
		[
			"DS:normal:GAUGE:1:0:800 ",
			"DS:dirty:GAUGE:1:0:800 ",
			"DS:error:ABSOLUTE:1:0:1 ",
			plan(Plan)
		]
	]),
	Result = os:cmd(Command),
	io:format("~s~n~n~s~n", [Command, Result]),
	true.

plan(Plan) ->
	plan(Plan, []).

plan([], []) ->
	<<>>;
plan([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
plan([Test | Rest], Acc) ->
	plan(Rest, [$\s, << "DS:", Test/binary, ":ABSOLUTE:1:0:1" >> | Acc]).

template(Event) ->
	template(Event, []).

template([], []) ->
	<<>>;
template([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
template([{Key, _} | Event], Acc) ->
	template(Event, [$:, Key | Acc]).

unixtime() ->
	unixtime(os:timestamp()).

unixtime({A, B, _}) ->
	(A * 1000000) + B.

values(Event) ->
	values(Event, []).

values([], []) ->
	<<>>;
values([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
values([{_, Value} | Event], Acc) ->
	values(Event, [$:, erlang:integer_to_binary(Value) | Acc]).

% values(Data) ->
% 	values(Data, #{ ns => 0, ds => 0 }).

% values([], #{ ns := NS, ds := DS }) ->
% 	<< (erlang:integer_to_binary(NS))/binary, $:, (erlang:integer_to_binary(DS))/binary >>;
% values([{I, V0} | Rest], Acc=#{ ds := DS0 }) when I >= 9 ->
% 	V1 = erlang:round(V0 * 100),
% 	DS1 = DS0 + V1,
% 	values(Rest, Acc#{ ds := DS1 });
% values([{_I, V0} | Rest], Acc=#{ ns := NS0 }) ->
% 	V1 = erlang:round(V0 * 100),
% 	NS1 = NS0 + V1,
% 	values(Rest, Acc#{ ns := NS1 }).
