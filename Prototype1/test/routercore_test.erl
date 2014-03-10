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

	{ok, _PID} = generic_exchange_dialog_router:start_link(
		ets:new(dialogETS, [public, bag])),

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

	?assertEqual({ok, routed}, generic_exchange_dialog_router:route_message(MCall)).




	

