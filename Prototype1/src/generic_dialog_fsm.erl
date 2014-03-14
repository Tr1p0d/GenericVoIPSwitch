-module(generic_dialog_fsm).
-behaviour(gen_fsm).

-export([start_link/1, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([idle/3, ringing/3, dialed/3]).

-include("../include/generic_exchange.hrl").



%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromRP, MSG=#generic_msg{type=associate}}, _From, _Destination) ->
	gen_server:reply(_Destination, {route_message, route(MSG)}),
	{reply, ok, dialed, _Destination};

%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromTU, MSG=#generic_msg{type=associate}}, _From, _Destination) ->
	gen_server:reply(_Destination, {route_message, route(MSG)}),
	{reply, ok, dialed, _Destination};

%% ------------ IDLE -> DIALED TRANSITION
idle({fromTU, MSG=#generic_msg{type=make_call}}, _From, _Destination) ->
	gen_server:reply(_Destination, {route_message, route(MSG)}),
	{reply, ok, dialed, _Destination};

%% ------------ IDLE -> RINGING TRANSITION
idle({fromRP, _MSG=#generic_msg{type=make_call}}, _From, _Destination) ->
	{reply, ok, ringing, _Destination}.

dialed(_Msg, _From, _Dest) ->
	ok.

ringing(_Msg, _From, _Dest) ->
	ok.

start_link(Destination) ->
	gen_fsm:start_link(?MODULE, Destination, []).

init(Destination) ->
	{ok, idle, Destination}.	

code_change(_Old, StateName, StateData, _Extra)->
	{ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
	lager:warning('received all state event'),
	{next_state, StateName, StateData}.

handle_info(_Event, StateName, StateData)->
	lager:warning('received unidentified message ~p', [_Event]),
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	lager:warning('received all sync state event'),
	{reply, StateName, StateName, StateData}.

terminate(_Reason, StateName, _StateData) ->
	lager:warning("?MODULE terminatedi while in ~p", [StateName]),
	ok.

route(_MSG=#generic_msg{      
	type             = make_call,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	specificProtocol = SpecProt }) ->
	
	#generic_msg{
		type             = make_call,
		target 		     = Target,
		caller 		     = Caller,
		callee 		     = Callee,
		upstreamRoute  	 = USRoute,
		downstreamRoute  = DSRoute ++ [<<"10.10.10.10">>],
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		specificProtocol = SpecProt };


%% successfull registration
route(_MSG=#generic_msg{      
	type             = associate,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	specificProtocol = SpecProt }) ->
	
	#generic_msg{
		type             = accept,
		target 		     = Target,
		caller 		     = Caller,
		callee 		     = Callee,
		upstreamRoute  	 = USRoute,
		downstreamRoute  = DSRoute ++ [<<"10.10.10.10">>],
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		specificProtocol = SpecProt }.
