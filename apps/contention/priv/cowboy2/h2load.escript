#!/usr/bin/env escript
%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(h2load).
-export([main/1]).

-define(contention, 'contention@127.0.0.1').

main([Node, Cookie, Executable, Filename]) ->
	{ok, _} = net_kernel:start([erlang:list_to_atom(Node), longnames]),
	true = erlang:set_cookie(erlang:node(), erlang:list_to_atom(Cookie)),
	Parent = erlang:self(),
	stats = ets:new(stats, [named_table, public, duplicate_bag]),
	Worker = erlang:spawn_link(fun() -> init(Parent, Executable, Filename) end),
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
	io:format("usage: h2load.escript NODE COOKIE EXECUTABLE FILENAME\n"),
	erlang:halt(1).

-record(state, {
	parent = nil :: pid(),
	poll = nil :: pid(),
	test = nil :: pid(),
	filename = nil :: string(),
	executable = nil :: string(),
	port = nil :: port(),
	kill_cmd = nil :: string()
}).

init(Parent, Executable, Filename) ->
	Self = erlang:self(),
	Poll = erlang:spawn_link(fun () -> node_poll_init(Self) end),
	% Test = erlang:spawn_link(fun () -> node_test_init(Self) end),
	Test = erlang:spawn_link(fun () -> stop_after_5_minutes(Self) end),
	State0 = #state{
		parent = Parent,
		poll = Poll,
		test = Test,
		filename = Filename,
		executable = Executable
	},
	State1 = start_h2load(State0),
	% State1 = State0,
	loop(State1).

loop(State=#state{parent=Parent, poll=Poll, test=Test, port=Port}) ->
	receive
		{Port, {data, {eol, Line}}} ->
			handle_line(State, Line);
		{Port, {data, _}} ->
			loop(State);
		{Port, {exit_status, 0}} ->
			loop(start_h2load(State#state{port=nil, kill_cmd=nil}));
		stop ->
			_ = erlang:send(Poll, stop),
			receive
				{stopped, Poll} ->
					ok
			after 100 ->
				catch erlang:exit(Poll, kill)
			end,
			_ = erlang:send(Test, stop),
			receive
				{stopped, Test} ->
					ok
			after 100 ->
				catch erlang:exit(Test, kill)
			end,
			ok = terminate(State),
			Stats = reduce_stats(),
			ok = write_stats(State, Stats),
			_ = erlang:send(Parent, {stopped, erlang:self()}),
			ok;
		Message ->
			io:format("M: ~p~n", [Message]),
			loop(State)
	end.

handle_line(State, << "RPSI ", Timestamp0:19/binary, $\s, RPS0/binary >>) ->
	try erlang:binary_to_integer(Timestamp0) of
		Timestamp ->
			try erlang:binary_to_float(RPS0) of
				RPS ->
					true = write(requests, Timestamp, RPS),
					loop(State)
			catch _:_ ->
				loop(State)
			end
	catch _:_ ->
		loop(State)
	end;
handle_line(State, _) ->
	loop(State).

terminate(#state{port=Port, kill_cmd=KillCmd}) when is_port(Port) andalso KillCmd =/= nil ->
	catch erlang:port_close(Port),
	io:format("~s -> ~s", [KillCmd, os:cmd(KillCmd)]),
	ok;
terminate(_) ->
	ok.

%%%-------------------------------------------------------------------
%%% h2load functions
%%%-------------------------------------------------------------------

%% @private
start_h2load(State0=#state{executable=Executable}) ->
	Port = erlang:open_port({spawn_executable, "./h2load-spawn.sh"}, [
		{line, 1024},
		{args, [
			Executable,
			"--duration=300",
			"--warm-up-time=1s",
			"--interval=1s",
			"--clients=100",
			"--max-concurrent-streams=10",
			"--requests=0",
			"--window-bits=16",
			"--connection-window-bits=16",
			"http://127.0.0.1:29593/"
		]},
		exit_status,
		use_stdio,
		stderr_to_stdout,
		binary
	]),
	State1 = State0#state{port=Port},
	wait_h2load(State1).

%% @private
wait_h2load(State=#state{port=Port}) ->
	receive
		{Port, {data, {eol, KillCmd = <<"kill", _/binary>>}}} ->
			State#state{kill_cmd=erlang:binary_to_list(KillCmd)};
		{Port, Err} ->
			catch erlang:port_close(Port),
			erlang:throw({os_process_error, Err})
	after 5000 ->
		catch erlang:port_close(Port),
		erlang:throw({os_process_error, "OS process timed out."})
	end.

%%%-------------------------------------------------------------------
%%% node poll functions
%%%-------------------------------------------------------------------

-record(poll, {
	parent = nil :: pid(),
	node = nil :: node()
}).

node_poll_init(Parent) ->
	Node = ?contention,
	State = #poll{
		parent = Parent,
		node = Node
	},
	ok = connect(Node),
	node_poll_loop(State).

node_poll_loop(State=#poll{parent=Parent, node=Node}) ->
	receive
		{contention, {wall_time, Timestamp, Data0}} ->
			{N, D} = scheduler_utilization(Data0),
			true = write(normal, Timestamp, N),
			true = write(dirty, Timestamp, D),
			% true = ets:insert(stats, [{schedulers, {Timestamp, Data}}]),
			% io:format("C: ~w ~p~n", [Timestamp, Data]),
			node_poll_loop(State);
		stop ->
			ok = disconnect(Node),
			_ = erlang:send(Parent, {stopped, erlang:self()}),
			ok;
		Message ->
			io:format("M: ~p~n", [Message]),
			node_poll_loop(State)
	after 1000 ->
		{Timestamp, Data0} = rpc:call(Node, 'Elixir.Contention.SchedulerUtilization', fetch, []),
		{N, D} = scheduler_utilization(Data0),
		true = write(normal, Timestamp, N),
		true = write(dirty, Timestamp, D),
		% true = ets:insert(stats, [{schedulers, {Timestamp, Data}}]),
		% io:format("R: ~w ~p~n", [Timestamp, Data]),
		node_poll_loop(State)
	end.

%%%-------------------------------------------------------------------
%%% node test functions
%%%-------------------------------------------------------------------

stop_after_5_minutes(Parent) ->
	TRef = erlang:start_timer(timer:minutes(5), erlang:self(), stop),
	stop_after_5_minutes_loop(Parent, TRef).

stop_after_5_minutes_loop(Parent, TRef) ->
	receive
		{timeout, TRef, stop} when is_reference(TRef) ->
			_ = erlang:send(Parent, stop),
			stop_after_5_minutes_loop(Parent, nil);
		stop ->
			_ = erlang:send(Parent, {stopped, erlang:self()}),
			ok;
		Message ->
			io:format("M: ~p~n", [Message]),
			stop_after_5_minutes_loop(Parent, TRef)
	after 10000 ->
		io:format("ms remaining: ~p~n", [erlang:read_timer(TRef)]),
		stop_after_5_minutes_loop(Parent, TRef)
	end.

-record(test, {
	parent = nil :: pid(),
	node = nil :: node(),
	wait = timer:seconds(10) :: timeout(),
	wait_ref = nil :: reference(),
	startup = nil :: integer(),
	test = nil :: binary(),
	test_ref = nil :: reference(),
	tests = [] :: [{binary(), {module(), atom(), [term()]}}]
}).

node_test_init(Parent) ->
	Node = ?contention,
	Function = spinsleep,
	Wait = 1000 * 1,
	Sched = erlang:system_info(schedulers),
	State0 = #test{
		parent = Parent,
		node = Node,
		startup = erlang:system_time(nanosecond),
		tests = [
			{<<"1x">>, {'Elixir.Contention', Function, [Wait, Sched * 1]}},
			{<<"10x">>, {'Elixir.Contention', Function, [Wait, Sched * 10]}},
			{<<"100x">>, {'Elixir.Contention', Function, [Wait, Sched * 100]}},
			{<<"1000x">>, {'Elixir.Contention', Function, [Wait, Sched * 1000]}},
			{<<"10000x">>, {'Elixir.Contention', Function, [Wait, Sched * 10000]}}
		]
	},
	State1 = node_test_wait(State0),
	node_test_loop(State1).

node_test_loop(State0=#test{parent=Parent, test_ref=TestRef, wait_ref=WaitRef}) ->
	receive
		{timeout, WaitRef, wait} when is_reference(WaitRef) ->
			State1 = State0#test{wait_ref=nil},
			node_test(State1);
		{TestRef, _Result} when is_reference(TestRef) ->
			true = write(State0#test.test, erlang:system_time(nanosecond), 'end'),
			State1 = node_test_wait(State0),
			node_test_loop(State1);
		stop ->
			_ = erlang:send(Parent, {stopped, erlang:self()}),
			ok;
		Message ->
			io:format("M: ~p~n", [Message]),
			node_test_loop(State0)
	end.

%% @private
node_test(State0=#test{node=Node, tests=[{Test, {M, F, A}} | Tests]}) ->
	io:format("starting ~s~n", [Test]),
	TestRef = erlang:make_ref(),
	Self = erlang:self(),
	_ = erlang:spawn(fun() ->
		Result = rpc:block_call(Node, M, F, A),
		Self ! {TestRef, Result},
		erlang:exit(normal)
	end),
	true = write(Test, erlang:system_time(nanosecond), start),
	State1 = State0#test{test=Test, test_ref=TestRef, tests=Tests},
	node_test_loop(State1);
node_test(State=#test{parent=Parent, tests=[]}) ->
	_ = erlang:send(Parent, stop),
	node_test_loop(State).

%% @private
node_test_wait(State=#test{wait=Wait, wait_ref=nil}) ->
	WaitRef = erlang:start_timer(Wait, erlang:self(), wait),
	State#test{wait_ref=WaitRef};
node_test_wait(State=#test{wait_ref=WaitRef}) ->
	catch erlang:cancel_timer(WaitRef),
	ok = receive {timeout, WaitRef, wait} -> ok after 0 -> ok end,
	node_test_wait(State#test{wait_ref=nil}).

%%%-------------------------------------------------------------------
%%% node functions
%%%-------------------------------------------------------------------

connect(Node) ->
	true = net_kernel:connect_node(Node),
	ok = rpc:call(Node, contention_event, add_handler, [contention_event_handler, erlang:self()]),
	ok.

disconnect(Node) ->
	ok = rpc:call(Node, contention_event, delete_handler, [contention_event_handler, erlang:self()]),
	ok.

scheduler_utilization({N, D}) ->
	{
		round_usage(N),
		round_usage(D)
	}.

%%%-------------------------------------------------------------------
%%% time series functions
%%%-------------------------------------------------------------------

reduce_stats() ->
	Stats0 = lists:sort(ets:match_object(stats, '_')),
	{Tests0, Stats1} = reduce_tests(Stats0, #{}, []),
	{Usage, Stats2} = reduce_usage(Stats1, [], []),
	Tests = tests_to_stats(maps:to_list(Tests0), []),
	{Requests, []} = reduce_requests(Stats2, [], []),
	Stats3 = combine_stats(lists:flatten([Usage, Tests, Requests]), #{}),
	% io:format("Tests:    ~p~n", [Tests]),
	% io:format("Usage:    ~p~n", [Usage]),
	% io:format("Requests: ~p~n", [Requests]),
	% io:format("Stats: ~p~n", [Stats3]),
	Stats3.

combine_stats([], Acc) ->
	lists:sort(maps:to_list(Acc));
combine_stats([{TS, K, V} | Stats], Acc) ->
	case maps:find(TS, Acc) of
		{ok, Event0} ->
			Event1 = maps:put(K, V, Event0),
			combine_stats(Stats, maps:put(TS, Event1, Acc));
		error ->
			Event = maps:put(K, V, maps:new()),
			combine_stats(Stats, maps:put(TS, Event, Acc))
	end.

tests_to_stats([], Acc) ->
	lists:sort(Acc);
tests_to_stats([{Test, {Start, End}} | Tests], Acc) ->
	tests_to_stats(Tests, test_to_stats(Test, Start, End, Acc)).

test_to_stats(Test, End, End, Acc) ->
	[{End, Test, 1} | Acc];
test_to_stats(Test, Start, End, Acc) when Start < End ->
	test_to_stats(Test, Start + 1, End, [{Start, Test, 1} | Acc]).

reduce_tests([], Tests, Acc) ->
	{Tests, lists:reverse(Acc)};
reduce_tests([{Bucket, {Test, _, start}} | Stats], Tests, Acc) when is_binary(Test) ->
	reduce_tests(Stats, maps:put(Test, Bucket, Tests), Acc);
reduce_tests([{Bucket, {Test, _, 'end'}} | Stats], Tests0, Acc) when is_binary(Test) ->
	Tests1 =
		case maps:find(Test, Tests0) of
			{ok, Start} when (Bucket - Start) < 1->
				maps:put(Test, {Start, Bucket + 1}, Tests0);
			{ok, Start} ->
				maps:put(Test, {Start, Bucket}, Tests0);
			_ ->
				Tests0
		end,
	reduce_tests(Stats, Tests1, Acc);
reduce_tests([Stat | Stats], Tests, Acc) ->
	reduce_tests(Stats, Tests, [Stat | Acc]).

reduce_requests([], Requests, Acc) ->
	{fill_requests_gaps(lists:reverse(Requests), []), lists:reverse(Acc)};
reduce_requests([{Bucket, {requests, _, V0}} | Stats0], Requests, Acc) ->
	{V, Stats1} = reduce_request(Stats0, Bucket, V0),
	reduce_requests(Stats1, [{Bucket, requests, V} | Requests], Acc);
reduce_requests([Stat | Stats], Requests, Acc) ->
	reduce_requests(Stats, Requests, [Stat | Acc]).

reduce_request([{Bucket, {requests, _, V0}} | Stats], Bucket, V1) ->
	reduce_request(Stats, Bucket, (V0 + V1) / 2);
reduce_request(Stats, _, V) ->
	{V, Stats}.

fill_requests_gaps([], Acc) ->
	lists:sort(Acc);
fill_requests_gaps([
	{TS0, K, V0},
	{TS1, K, V1}
	| Requests0
], Acc0) ->
	Acc1 =
		case TS1 - TS0 of
			Diff when Diff > 1 ->
				[
					{TS0, K, V0}
					| fill_average(TS0, Diff - 1, K, (V0 + V1) / 2, Acc0)
				];
			1 ->
				[
					{TS0, K, V0}
					| Acc0
				]
		end,
	Requests1 = [
		{TS1, K, V1}
		| Requests0
	],
	fill_requests_gaps(Requests1, Acc1);
fill_requests_gaps([Stat | Requests], Acc) ->
	fill_requests_gaps(Requests, [Stat | Acc]).

reduce_usage([], Usage, Acc) ->
	{fill_usage_gaps(lists:reverse(Usage), []), lists:reverse(Acc)};
reduce_usage([{Bucket, {K, _, V0}} | Stats0], Usage, Acc) when K == normal orelse K == dirty ->
	{V1, Stats1} = reduce_usage_type(Stats0, Bucket, K, V0),
	reduce_usage(Stats1, [{Bucket, K, V1} | Usage], Acc);
reduce_usage([Stat | Stats], Usage, Acc) ->
	reduce_usage(Stats, Usage, [Stat | Acc]).

reduce_usage_type([{Bucket, {K, _, V0}} | Stats], Bucket, K, V1) ->
	reduce_usage_type(Stats, Bucket, K, erlang:max(V0, V1));
reduce_usage_type(Stats, _, _, V) ->
	{V, Stats}.

fill_usage_gaps([], Acc) ->
	lists:sort(Acc);
fill_usage_gaps([
	{TS0, Ka, Va0},
	{TS0, Kb, Vb0},
	{TS1, Ka, Va1},
	{TS1, Kb, Vb1}
	| Usage0
], Acc0) ->
	Acc1 =
		case (TS1 - TS0) of
			Diff when Diff > 2 ->
				[
					{TS0, Ka, Va0},
					{TS0, Kb, Vb0}
					| fill_error(TS0, Diff - 1, Acc0)
				];
			2 ->
				[
					{TS0, Ka, Va0},
					{TS0, Kb, Vb0},
					{TS0 + 1, Ka, round_usage((Va0 + Va1) / 2)},
					{TS0 + 1, Kb, round_usage((Vb0 + Vb1) / 2)}
					| Acc0
				];
			1 ->
				[
					{TS0, Ka, Va0},
					{TS0, Kb, Vb0}
					| Acc0
				]
		end,
	Usage1 = [
		{TS1, Ka, Va1},
		{TS1, Kb, Vb1}
		| Usage0
	],
	fill_usage_gaps(Usage1, Acc1);
fill_usage_gaps([Stat | Usage], Acc) ->
	fill_usage_gaps(Usage, [Stat | Acc]).

fill_average(TS, I, K, V, Acc) when I > 0 ->
	fill_average(TS, I - 1, K, V, [{TS + I, K, V} | Acc]);
fill_average(_TS, _I, _K, _V, Acc) ->
	Acc.

fill_error(TS, I, Acc) when I > 0 ->
	fill_error(TS, I - 1, [{TS + I, error, 1} | Acc]);
fill_error(_TS, _I, Acc) ->
	Acc.

round_usage(X) ->
	erlang:round(X * 10000) / 10000.

write(Key, Timestamp, Value) ->
	Bucket = erlang:round(Timestamp / 1000000000),
	true = ets:insert(stats, {Bucket, {Key, Timestamp, Value}}).

write_stats(State, Stats) ->
	true = rrdtool_create(State, Stats),
	ok = rrdtool_update(State, Stats),
	ok = rrdtool_graph0(State, Stats),
	ok = rrdtool_graph1(State, Stats),
	ok.

%%%-------------------------------------------------------------------
%%% rrdtool functions
%%%-------------------------------------------------------------------

rrdtool_create(#state{filename=Filename}, Stats = [{TS, _} | _]) ->
	Command = io_lib:format(
		"rrdtool create \"~s.rrd\""
		" --start ~w"
		" --step 1"
		" DS:normal:GAUGE:1:0:1"
		" DS:dirty:GAUGE:1:0:1"
		" DS:error:ABSOLUTE:1:0:1"
		" DS:requests:GAUGE:1:0:1000000"
		" ~s"
		" RRA:MAX:0:1:7200",
		[
			Filename,
			TS - 1,
			rrdtool_datasets(Stats)
		]
	),
	Result = os:cmd(Command),
	io:format("~s~n~n~s~n", [Command, Result]),
	true;
rrdtool_create(_, _) ->
	false.

rrdtool_datasets(Stats) ->
	rrdtool_datasets_uniq(Stats, #{}).

rrdtool_datasets([], []) ->
	<<>>;
rrdtool_datasets([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_datasets([K | Rest], Acc) ->
	Dataset = io_lib:format(
		"DS:~s:ABSOLUTE:1:0:1",
		[
			K
		]
	),
	rrdtool_datasets(Rest, [$\s, Dataset | Acc]).

rrdtool_datasets_uniq([], Acc) ->
	rrdtool_datasets(lists:sort(maps:keys(Acc)), []);
rrdtool_datasets_uniq([{_, M} | Stats], Acc0) ->
	Acc1 = lists:foldl(fun
		(K, A) when is_binary(K) ->
			maps:put(K, [], A);
		(_, A) ->
			A
	end, Acc0, maps:keys(M)),
	rrdtool_datasets_uniq(Stats, Acc1);
rrdtool_datasets_uniq([_ | Stats], Acc) ->
	rrdtool_datasets_uniq(Stats, Acc).

rrdtool_update(State, Stats) ->
	Groups = rrdtool_update_groups(Stats),
	rrdtool_updates(State, Groups).

rrdtool_updates(_, []) ->
	ok;
rrdtool_updates(State=#state{filename=Filename}, [Group | Groups]) ->
	Template = rrdtool_update_template(Group),
	Values = rrdtool_update_values(Group),
	Command = io_lib:format(
		"rrdtool update \"~s.rrd\""
		" --template ~s"
		" ~s",
		[
			Filename,
			Template,
			Values
		]
	),
	Result = os:cmd(Command),
	io:format("~s~n~n~s~n", [Command, Result]),
	rrdtool_updates(State, Groups).

rrdtool_update_template([{_, Event} | _]) ->
	rrdtool_update_template(lists:sort(maps:keys(Event)), []).

rrdtool_update_template([], []) ->
	<<>>;
rrdtool_update_template([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_update_template([Key0 | Keys], Acc) ->
	Key =
		case Key0 of
			_ when is_atom(Key0) ->
				erlang:atom_to_binary(Key0, unicode);
			_ when is_binary(Key0) ->
				Key0
		end,
	rrdtool_update_template(Keys, [$:, Key | Acc]).

rrdtool_update_values(Group) ->
	rrdtool_update_values(Group, []).

rrdtool_update_values([], []) ->
	<<>>;
rrdtool_update_values([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_update_values([{TS, Event0} | Group], Acc) ->
	Event = lists:sort(maps:to_list(Event0)),
	Value = rrdtool_update_event([{timestamp, TS} | Event]),
	rrdtool_update_values(Group, [$\s, Value | Acc]).

rrdtool_update_event(Event) ->
	rrdtool_update_event(Event, []).

rrdtool_update_event([], []) ->
	<<>>;
rrdtool_update_event([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_update_event([{_, V} | Event], Acc) ->
	Value =
		case V of
			_ when is_float(V) ->
				io_lib:format("~w", [V]);
			_ when is_integer(V) ->
				erlang:integer_to_binary(V)
		end,
	rrdtool_update_event(Event, [$:, Value | Acc]).

rrdtool_update_groups([{TS, Event} | Stats]) ->
	Keys = lists:sort(maps:keys(Event)),
	rrdtool_update_groups(Stats, Keys, [], [{TS, Event}]).

rrdtool_update_groups([], _, Groups, Acc) ->
	Group = lists:reverse(Acc),
	lists:reverse([Group | Groups]);
rrdtool_update_groups([{TS, Event} | Stats], Keys, Groups, Acc) ->
	case lists:sort(maps:keys(Event)) of
		Keys ->
			rrdtool_update_groups(Stats, Keys, Groups, [{TS, Event} | Acc]);
		NewKeys ->
			Group = lists:reverse(Acc),
			rrdtool_update_groups(Stats, NewKeys, [Group | Groups], [{TS, Event}])
	end.

%%%-------------------------------------------------------------------
%%% rrdtool graph0 functions
%%%-------------------------------------------------------------------

rrdtool_graph0(_State=#state{filename=Filename}, Stats = [{Start, _} | _]) ->
	{End, _} = lists:last(Stats),
	Current = erlang:system_time(second),
	StartOfDay = (Current - (Current rem 86400)) + 21600, % convert to midnight MDT
	Shift = StartOfDay - Start,
	Seconds = End - Start,
	Command = io_lib:format(
		"rrdtool graph ~s.0.svg \\~n"
		"--width 600 \\~n"
		"--height 200 \\~n"
		"--start 00:00 \\~n"
		"--end start+~wseconds \\~n"
		"--title '~s' \\~n"
		"--vertical-label 'scheduler usage' \\~n"
		"--imgformat SVG \\~n"
		"--border 0 \\~n"
		"--font DEFAULT:0:Consolas \\~n"
		"--upper-limit 1 \\~n"
		"--lower-limit -1 \\~n"
		"--rigid \\~n"
		"~s \\~n"
		"~s \\~n"
		"'CDEF:normal=normal0,1,/' \\~n"
		"'CDEF:dirty=dirty0,1,/,-1,*' \\~n"
		"'CDEF:ln1=normal,normal,UNKN,IF' \\~n"
		"'CDEF:ln2=dirty,dirty,UNKN,IF' \\~n"
		"~s \\~n"
		"'TICK:error#e60073a0:1:  Error' \\~n"
		"'AREA:normal#48c4eca0: Normal' \\~n"
		"'AREA:dirty#54ec48a0: Dirty' \\~n"
		"'LINE1:ln1#1598c3' \\~n"
		"'LINE1:ln2#24bc14' \\~n"
		"'HRULE:0#000000:dashes=3,5:dash-offset=5' ~n"
		, [
			Filename,
			Seconds,
			Filename,
			rrdtool_graph0_def(Stats, Filename, Start, End),
			rrdtool_graph0_shift(Stats, Shift),
			rrdtool_graph0_tick(Stats)
		]
	),
	file:write_file(Filename ++ ".0.sh", "#!/usr/bin/env bash\n\n" ++ Command),
	_ = os:cmd("chmod +x " ++ Filename ++ ".0.sh"),
	_Result = os:cmd(Command),
	% io:format("~s~n~n~s~n", [Command, Result]),
	ok.

rrdtool_graph0_def(Stats, Filename, Start, End) ->
	Keys = rrdtool_graph0_keys(Stats, #{}),
	rrdtool_graph0_def(Keys, Filename, Start, End, []).

rrdtool_graph0_def([], _, _, _, []) ->
	<<>>;
rrdtool_graph0_def([], _, _, _, [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph0_def([Key | Keys], Filename, Start, End, Acc0) ->
	Source =
		case Key of
			_ when is_atom(Key) ->
				erlang:atom_to_binary(Key);
			_ when is_binary(Key) ->
				Key
		end,
	Label =
		case Key of
			<<"normal">> ->
				<<"normal0">>;
			<<"dirty">> ->
				<<"dirty0">>;
			_ ->
				Source
		end,
	Value = io_lib:format("'DEF:~s=~s.rrd:~s:MAX:start=~w:end=~w:step=1'", [
		Label,
		Filename,
		Source,
		Start,
		End
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	rrdtool_graph0_def(Keys, Filename, Start, End, Acc1).

rrdtool_graph0_keys([], Acc) ->
	lists:usort([<<"error">> | maps:keys(Acc)]);
rrdtool_graph0_keys([{_, Event} | Stats], Acc0) ->
	Acc1 = lists:foldl(fun
		(K, A) when K == normal orelse K == dirty orelse K == error -> maps:put(erlang:atom_to_binary(K, unicode), [], A);
		(K, A) when is_binary(K) -> maps:put(K, [], A);
		(_, A) -> A
	end, Acc0, lists:sort(maps:keys(Event))),
	rrdtool_graph0_keys(Stats, Acc1).

rrdtool_graph0_shift(Stats, Shift) ->
	Keys = rrdtool_graph0_keys(Stats, #{}),
	rrdtool_graph0_shift(Keys, Shift, []).

rrdtool_graph0_shift([], _, []) ->
	<<>>;
rrdtool_graph0_shift([], _, [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph0_shift([Key | Keys], Shift, Acc0) ->
	Label =
		case Key of
			<<"normal">> ->
				<<"normal0">>;
			<<"dirty">> ->
				<<"dirty0">>;
			_ when is_binary(Key) ->
				Key
		end,
	Value = io_lib:format("'SHIFT:~s:~w'", [
		Label,
		Shift
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	rrdtool_graph0_shift(Keys, Shift, Acc1).

rrdtool_graph0_tick(Stats) ->
	Keys0 = rrdtool_graph0_keys(Stats, #{}) -- [<<"normal">>, <<"dirty">>, <<"error">>],
	Keys = lists:sort(fun (A, B) ->
		[A0 | _] = binary:split(A, <<"x">>),
		[B0 | _] = binary:split(B, <<"x">>),
		Ai = erlang:binary_to_integer(A0),
		Bi = erlang:binary_to_integer(B0),
		Ai =< Bi
	end, Keys0),
	rrdtool_graph0_tick(Keys, []).

rrdtool_graph0_tick([], []) ->
	<<>>;
rrdtool_graph0_tick([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph0_tick([Key | Keys], Acc0) ->
	Tick = io_lib:format("'TICK:~s#00000020:1:  ~s'", [
		Key,
		Key
	]),
	Acc1 = [<<" \\\n">>, Tick | Acc0],
	rrdtool_graph0_tick(Keys, Acc1).

%%%-------------------------------------------------------------------
%%% rrdtool graph1 functions
%%%-------------------------------------------------------------------

rrdtool_graph1(_State=#state{filename=Filename}, Stats = [{Start, _} | _]) ->
	{End, _} = lists:last(Stats),
	Current = erlang:system_time(second),
	StartOfDay = (Current - (Current rem 86400)) + 21600, % convert to midnight MDT
	Shift = StartOfDay - Start,
	Seconds = End - Start,
	Command = io_lib:format(
		"rrdtool graph ~s.1.svg \\~n"
		"--width 600 \\~n"
		"--height 200 \\~n"
		"--start 00:00 \\~n"
		"--end start+~wseconds \\~n"
		"--title '~s' \\~n"
		"--vertical-label 'requests per second' \\~n"
		"--imgformat SVG \\~n"
		"--border 0 \\~n"
		"--font DEFAULT:0:Consolas \\~n"
		"--upper-limit 70000 \\~n"
		"--lower-limit 0 \\~n"
		"--rigid \\~n"
		"~s \\~n"
		"~s \\~n"
		"'CDEF:ln1=requests,requests,UNKN,IF' \\~n"
		"~s \\~n"
		"'TICK:error#e60073a0:1:  Error' \\~n"
		"'AREA:requests#7648eca0: req/s\\l' \\~n"
		"'LINE1:ln1#4d18e4' \\~n"
		% "'CDEF:requests10s=requests,10,TRENDNAN' \\~n"
		% "'LINE1:requests10s#000000' \\~n"
		"'VDEF:requestsmax=requests,MAXIMUM' \\~n"
		"'VDEF:requestsmin=requests,MINIMUM' \\~n"
		"'VDEF:requestsavg=requests,AVERAGE' \\~n"
		"'COMMENT:\\u' \\~n"
		"'GPRINT:requestsavg:AVG %6.0lf' \\~n"
		"'GPRINT:requestsmin:MIN %6.0lf' \\~n"
		"'GPRINT:requestsmax:MAX %6.0lf\\r'~n"
		, [
			Filename,
			Seconds,
			Filename,
			rrdtool_graph1_def(Stats, Filename, Start, End),
			rrdtool_graph1_shift(Stats, Shift),
			rrdtool_graph1_tick(Stats)
		]
	),
	file:write_file(Filename ++ ".1.sh", "#!/usr/bin/env bash\n\n" ++ Command),
	_ = os:cmd("chmod +x " ++ Filename ++ ".1.sh"),
	_Result = os:cmd(Command),
	% io:format("~s~n~n~s~n", [Command, Result]),
	ok.

rrdtool_graph1_def(Stats, Filename, Start, End) ->
	Keys = rrdtool_graph1_keys(Stats, #{}),
	rrdtool_graph1_def(Keys, Filename, Start, End, []).

rrdtool_graph1_def([], _, _, _, []) ->
	<<>>;
rrdtool_graph1_def([], _, _, _, [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph1_def([Key | Keys], Filename, Start, End, Acc0) ->
	Source =
		case Key of
			_ when is_atom(Key) ->
				erlang:atom_to_binary(Key);
			_ when is_binary(Key) ->
				Key
		end,
	Label = Source,
	Value = io_lib:format("'DEF:~s=~s.rrd:~s:MAX:start=~w:end=~w:step=1'", [
		Label,
		Filename,
		Source,
		Start,
		End
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	rrdtool_graph1_def(Keys, Filename, Start, End, Acc1).

rrdtool_graph1_keys([], Acc) ->
	lists:usort([<<"requests">>, <<"error">> | maps:keys(Acc)]);
rrdtool_graph1_keys([{_, Event} | Stats], Acc0) ->
	Acc1 = lists:foldl(fun
		(K, A) when K == requests orelse K == error -> maps:put(erlang:atom_to_binary(K, unicode), [], A);
		(K, A) when is_binary(K) -> maps:put(K, [], A);
		(_, A) -> A
	end, Acc0, lists:sort(maps:keys(Event))),
	rrdtool_graph1_keys(Stats, Acc1).

rrdtool_graph1_shift(Stats, Shift) ->
	Keys = rrdtool_graph1_keys(Stats, #{}),
	rrdtool_graph1_shift(Keys, Shift, []).

rrdtool_graph1_shift([], _, []) ->
	<<>>;
rrdtool_graph1_shift([], _, [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph1_shift([Key | Keys], Shift, Acc0) ->
	Label = Key,
	Value = io_lib:format("'SHIFT:~s:~w'", [
		Label,
		Shift
	]),
	Acc1 = [<<" \\\n">>, Value | Acc0],
	rrdtool_graph1_shift(Keys, Shift, Acc1).

rrdtool_graph1_tick(Stats) ->
	Keys0 = rrdtool_graph1_keys(Stats, #{}) -- [<<"requests">>, <<"error">>],
	Keys = lists:sort(fun (A, B) ->
		[A0 | _] = binary:split(A, <<"x">>),
		[B0 | _] = binary:split(B, <<"x">>),
		Ai = erlang:binary_to_integer(A0),
		Bi = erlang:binary_to_integer(B0),
		Ai =< Bi
	end, Keys0),
	rrdtool_graph1_tick(Keys, []).

rrdtool_graph1_tick([], []) ->
	<<>>;
rrdtool_graph1_tick([], [_ | Acc]) ->
	erlang:iolist_to_binary(lists:reverse(Acc));
rrdtool_graph1_tick([Key | Keys], Acc0) ->
	Tick = io_lib:format("'TICK:~s#00000020:1:  ~s'", [
		Key,
		Key
	]),
	Acc1 = [<<" \\\n">>, Tick | Acc0],
	rrdtool_graph1_tick(Keys, Acc1).
