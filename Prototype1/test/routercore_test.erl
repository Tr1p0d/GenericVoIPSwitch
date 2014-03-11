-module(routercore_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/generic_exchange.hrl").


dialog_resolution_test() ->

	MCall = #generic_msg{
		type = make_call,
		target = <<1072>>,
		caller = {<<1071>>, <<"callid1">>, <<"tag1">> },
		callee = {<<1072>>, <<"callid1">>, <<"tag2">> },
		upstreamRoute = [],
		downstreamRoute = [<<"not_important_for_now">>],
		routeToRecord = [],
		sequenceNum = 1,
		specificProtocol = [<<"not_important_for_now">>]
	},

	A = dict:new(),
	AA = dict:store(<<1071>>, <<"1.1.1.1">>, A),
	AAA = dict:store(<<1072>>, <<"1.1.1.2">>, AA),

	{ok, _PID} = generic_exchange_dialog_router:start_link(
		ets:new(dialogETS, [named_table, public, bag]), AAA),

	?assertEqual({ok, created}, generic_exchange_dialog_router:route_message(MCall)).

dialog_created_test() ->
	MCall = #generic_msg{
		type = make_call,
		target = <<1072>>,
		caller = {<<1071>>, <<"callid1">>, <<"tag1">> },
		callee = {<<1072>>, <<"callid1">>, <<"tag2">> },
		upstreamRoute = [],
		downstreamRoute = [<<"not_important_for_now">>],
		routeToRecord = [],
		sequenceNum = 1,
		specificProtocol = [<<"not_important_for_now">>]
	},

	?assertEqual({ok, transmitted}, generic_exchange_dialog_router:route_message(MCall)).

dialog_table_consistency_test() ->
	?assertEqual([[<<"tag1">>], [<<"tag2">>]], ets:match(dialogETS, {<<"callid1">>, '$2', '_'})).

dialog_fsm_turning_test() ->
	[[PID]] = ets:match(dialogETS, {<<"callid1">>, <<"tag1">>, '$1'}),
	?assertEqual(dialed, gen_fsm:sync_send_all_state_event(PID, {blabla})).

message_routed_test() ->
	?assertEqual({blabla}, c:flush()).






	

