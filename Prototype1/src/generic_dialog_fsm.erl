-module(generic_dialog_fsm).
-behaviour(gen_fsm).

-export([start_link/2, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([idle/3, idle/2, ringing/3, ringing/2, dialed/3, dialed/2, ringback/3, ringback/2]).

-include("../include/generic_exchange.hrl").

-define(TIMEOUT, 10000).

-record(dialog_state, {
		specific_gateway		:: pid(),
		associationAA			:: term()
	}).


%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromRP, MSG=#generic_msg{type=associate, callee={Identifier, _, _},
	receivedOn=RecvOn}}, From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	addAssociates(Identifier, RecvOn, AAA),
	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{stop, normal, ok, State#dialog_state{associationAA=AAA}};

%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromTU, _MSG=#generic_msg{type=associate}}, _From, State) ->
	{stop, normal, ok, State};

%% ------------ IDLE -> DIALED TRANSITION
idle({fromTU, MSG=#generic_msg{type=make_call, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=_AAA }) ->

	{reply, ok, dialed, State, ?TIMEOUT};

%% ------------ IDLE -> RINGING TRANSITION
idle({fromRP, MSG=#generic_msg{type=make_call, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=_AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, _AAA)}),
	{reply, ok, ringing, State#dialog_state{associationAA=_AAA}, ?TIMEOUT}.

idle(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.


%% ------------ RINGING -> RINGING TRANSITION
ringing({fromTU, MSG=#generic_msg{type=ring, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=_AAA }) ->

	{reply, ok, ringing, State, ?TIMEOUT};

%% ------------ RINGING -> INCALL TRANSITION
ringing({fromTU, MSG=#generic_msg{type=accept, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=_AAA }) ->

	{reply, ok, incall, State, ?TIMEOUT}.

ringing(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.

%% ------------ DIALED -> RINGBACK TRANSITION
dialed({fromRP, MSG=#generic_msg{type=ring, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, ringback, State, ?TIMEOUT}.

dialed(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.

%% ------------ RINGBACK -> INCALL TRANSITION
ringback({fromRP, MSG=#generic_msg{type=accept, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, incall, State, ?TIMEOUT}.

ringback(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.

start_link(Destination, AssociationAA) ->
 	gen_fsm:start_link(?MODULE, [Destination, AssociationAA], []).

init([{PID, _Reference}, AssociationAA]) ->
	{ok, idle, #dialog_state{specific_gateway=PID, associationAA=AssociationAA}, ?TIMEOUT}.	

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
	generic_exchange_dialog_manager:delete_dialog(self()),
	lager:info("transaction terminated because ~p in state ~p", [_Reason, StateName]),
	ok.

%route ringing

-spec route(#generic_msg{}, {inet:ip_address(), inet:port_number()}, ets:tid() | atom()) -> 
	{#generic_msg{}, inet:ip_address(), inet:port_number()}.

route(_MSG=#generic_msg{      
	type             = accept,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, _, _) ->

	[ _  | Rest ] = lists:reverse(DSRoute), 	
	[ {DSIP, DSPort} | _ ] = Rest,
	{ok,DSIPfinal} = inet_parse:ipv4_address(binary_to_list(DSIP)),

	{#generic_msg{
		type             = accept,
		target 		     = Target,
		caller 		     = Caller,
		callee 		     = Callee,
		upstreamRoute  	 = USRoute,
		downstreamRoute  = lists:reverse(Rest),
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		timeToLive		 = TTL - 1,
		specificProtocol = SpecProt }, DSIPfinal, DSPort};

%route ringing
route(_MSG=#generic_msg{      
	type             = ring,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, _, _) ->


	[ _  | Rest ] = lists:reverse(DSRoute), 	
	[ {DSIP, DSPort} | _ ] = Rest,
	{ok,DSIPfinal} = inet_parse:ipv4_address(binary_to_list(DSIP)),

	{#generic_msg{
		type             = ring,
		target 		     = Target,
		caller 		     = Caller,
		callee 		     = Callee,
		upstreamRoute  	 = USRoute,
		downstreamRoute  = lists:reverse(Rest),
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		timeToLive		 = TTL - 1,
		specificProtocol = SpecProt }, DSIPfinal, DSPort};


% route invite
route(_MSG=#generic_msg{      
	type             = make_call,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, {_IP, _Port}, AAA) ->

	case generic_exchange_networking:resolve_target(Target, AAA) of
		{ok, {IP, RemotePort}} ->
			{#generic_msg{
				type             = make_call,
				target 		     = Target,
				caller 		     = Caller,
				callee 		     = Callee,
				upstreamRoute  	 = USRoute,
				downstreamRoute  = [{<<"127.0.0.1">>, 5060}|DSRoute],
				routeToRecord 	 = R2R,
				sequenceNum	     = SeqNum,
				timeToLive		 = TTL - 1,
				specificProtocol = SpecProt }, IP, RemotePort}
	end;

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
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, {IP, Port}, _) ->

	{#generic_msg{
		type             = accept,
		target 		     = Target,
		caller 		     = Caller,
		callee 		     = Callee,
		upstreamRoute  	 = USRoute,
		downstreamRoute  = [{<<"127.0.0.1">>, 5060}|DSRoute],
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		timeToLive		 = TTL - 1,
		specificProtocol = SpecProt }, IP, Port}.


-spec addAssociates(term(), term(), ets:tid() | atom()) ->
	true.

addAssociates(Key, Item, DT) ->
	ets:insert(DT, {Key, Item}).

