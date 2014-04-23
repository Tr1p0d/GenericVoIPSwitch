-module(generic_dialog_fsm).
-behaviour(gen_fsm).

-export([start_link/2, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([idle/3, idle/2, ringing/3, ringing/2, dialed/3, dialed/2, ringback/3, ringback/2, incall/3, incall/2, teardown/3, teardown/2]).

-include("../include/generic_exchange.hrl").

-define(TIMEOUT, 100000).

-record(dialog_state, {
		specific_gateway		:: pid(),
		associationAA			:: term()
	}).


%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromRP, MSG=#generic_msg{type=associate, callee={Identifier, _, _},
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	addAssociates(Identifier, RecvOn, PID, AAA),
	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{stop, normal, ok, State#dialog_state{associationAA=AAA}};

%% ------------ IDLE -> IDLE TRANSITION --- ASSOCIATION
idle({fromTU, _MSG=#generic_msg{type=associate}}, _From, State) ->
	{stop, normal, ok, State};

%% ------------ IDLE -> DIALED TRANSITION
idle({fromTU, #generic_msg{type=make_call}},
	_From, State)->

	{reply, ok, dialed, State, ?TIMEOUT};

%% ------------ IDLE -> RINGING TRANSITION
idle({fromRP, MSG=#generic_msg{type=make_call,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, ringing, State#dialog_state{associationAA=AAA}, ?TIMEOUT};

%% ------------ IDLE -> IDLE TRANSITION --- bad message
idle({ _ , MSG=#generic_msg{}}, _From, State=#dialog_state{ specific_gateway=PID}) ->
	gen_server:call(PID, {transmit_generic_msg, generic_exchange_gen_message_factory:reject(MSG)}),
	{stop, normal, ok, State}.

idle(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.




%% ------------ ringing -> ringing transition
ringing({fromTU, #generic_msg{type=ring}}, _From, State) ->
	{reply, ok, ringing, State, ?TIMEOUT};

%% ------------ RINGING -> INCALL TRANSITION
ringing({fromTU, #generic_msg{type=accept}}, _From, State) ->
	{reply, ok, incall, State, ?TIMEOUT};

%% ------------ RINGING -> IDLE TRANSITION
ringing({fromTU, #generic_msg{type=reject}}, _From, State) ->

	{stop, normal, ok, State};

%% ------------ ringing -> ringing transition
ringing({fromRP, #generic_msg{type=make_call}}, _From, State) ->
	{reply, ok, ringing, State, ?TIMEOUT};

%% ------------ ringing -> IDLE transition
ringing({fromRP, MSG=#generic_msg{type=teardown ,receivedOn=RecvOn}},
	_From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{stop, normal, ok, State}.

ringing(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.

%% ------------ DIALED -> RINGBACK TRANSITION
dialed({fromRP, MSG=#generic_msg{type=ring,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, ringback, State, ?TIMEOUT}.

dialed(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.

%% ------------ RINGBACK -> INCALL TRANSITION
ringback({fromRP, MSG=#generic_msg{type=accept,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, incall, State, ?TIMEOUT};

%% ------------ RINGBACK -> RINGBACK TRANSITION
ringback({fromRP, MSG=#generic_msg{type=ring,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, ringback, State, ?TIMEOUT};

%% ------------ RINGBACK -> RINGBACK TRANSITION
ringback({fromTU ,MSG=#generic_msg{type=make_call,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, ringback, State, ?TIMEOUT};

%% ------------ RINGBACK -> IDLE TRANSITION
ringback({fromRP, MSG=#generic_msg{type=reject,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{stop, normal, ok, State#dialog_state{associationAA=AAA}};

%% ------------ RINGBACK -> TEARDOWN TRANSITION
ringback({fromTU ,#generic_msg{type=teardown}},
	_From, State=#dialog_state{}) ->

	{reply, ok, teardown, State, ?TIMEOUT}.

ringback(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.




incall({fromTU, MSG=#generic_msg{type=teardown,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, teardown, State, ?TIMEOUT};

incall({fromRP, #generic_msg{type=teardown}}, _From, State) ->
	{reply, ok, incall, State, ?TIMEOUT};

incall({fromTU, MSG=#generic_msg{type=accept,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, incall, State, ?TIMEOUT};

incall({fromRP, MSG=#generic_msg{type=accept,
	receivedOn=RecvOn}}, _From, State=#dialog_state{ specific_gateway=PID,
	associationAA=AAA }) ->

	gen_server:call(PID, {transmit_generic_msg, route(MSG, RecvOn, AAA)}),
	{reply, ok, incall, State, ?TIMEOUT}.

incall(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.



teardown({fromRP, #generic_msg{type=accept}}, _From, State) ->
	{stop, normal, ok, State}.

teardown(timeout, State) ->
	?WARNING("dialog timed-out in state ~p", [State]),
	{stop, normal, State}.





start_link(Destination, AssociationAA) ->
 	gen_fsm:start_link(?MODULE, [Destination, AssociationAA], []).

init([{PID, _Reference}, AssociationAA]) ->
	{ok, idle, #dialog_state{specific_gateway=PID, associationAA=AssociationAA}, ?TIMEOUT};

init([PID, AssociationAA]) ->
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

%route ringing
route(_MSG=#generic_msg{      
	type             = Type,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, _, AETS) when 
		Type == ring; Type == accept; Type == reject ->

	{CID, _DID, _PartID}  = Callee,

	lager:warning("DSROUTE in route ~p", [DSRoute]),

	[ _  | Rest ] = lists:reverse(DSRoute), 	
	case Rest of 
		% means THE exchange is destination. So lets see where to which GW shall
		% we pass the message
		[] ->
			case generic_exchange_networking:resolve_target(CID, AETS) of
				{ok, {IP, Port}} -> 
					{#generic_msg{
						type             = Type,
						target 		     = Target,
						caller 		     = Caller,
						callee 		     = Callee,
						upstreamRoute  	 = USRoute,
						downstreamRoute  = lists:reverse(Rest),
						routeToRecord 	 = R2R,
						sequenceNum	     = SeqNum,
						timeToLive		 = TTL - 1,
						specificProtocol = SpecProt }, IP, Port}
			end;

		% else just dummy routing based on downstream route
		[ {DSIP, DSPort, _Opts} | _ ] ->
			{ok,DSIPfinal} = inet_parse:ipv4_address(binary_to_list(DSIP)),
			{#generic_msg{
				type             = Type,
				target 		     = Target,
				caller 		     = Caller,
				callee 		     = Callee,
				upstreamRoute  	 = USRoute,
				downstreamRoute  = lists:reverse(Rest),
				routeToRecord 	 = R2R,
				sequenceNum	     = SeqNum,
				timeToLive		 = TTL - 1,
				specificProtocol = SpecProt }, DSIPfinal, DSPort}
	end;

% route invite
route(_MSG=#generic_msg{      
	type             = Type,
	target 		     = Target,
	caller 		     = Caller,
	callee 		     = Callee,
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, {_IP, _Port}, AAA) 
	when Type == make_call; Type == teardown ->

	case generic_exchange_networking:resolve_target(Target, AAA) of
		{ok, {IP, RemotePort}} ->
			{#generic_msg{
				type             = Type,
				target 		     = Target,
				caller 		     = Caller,
				callee 		     = Callee,
				upstreamRoute  	 = USRoute,
				downstreamRoute  = [{generic_exchange_networking:get_domain(),
					generic_exchange_networking:get_port(),
					[{<<"branch">>,nksip_lib:uid()}]}|DSRoute],
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
	callee 		     = {Identifier, DialogID, _DialogPart},
	upstreamRoute  	 = USRoute,
	downstreamRoute  = DSRoute,
	routeToRecord 	 = R2R,
	sequenceNum	     = SeqNum,
	timeToLive		 = TTL,
	specificProtocol = SpecProt }, {IP, Port}, _) ->

	{#generic_msg{
		type             = accept,
		target 		     = Target,
		callee 		     = Caller,
		caller 		     = {Identifier, DialogID, nksip_lib:uid()},
		upstreamRoute  	 = USRoute,
		downstreamRoute  = DSRoute,
		routeToRecord 	 = R2R,
		sequenceNum	     = SeqNum,
		timeToLive		 = TTL - 1,
		specificProtocol = SpecProt }, IP, Port}.


-spec addAssociates(term(), term(), term(), ets:tid() | atom()) ->
	true.

addAssociates(Key, Item, Item2, DT) ->
	ets:insert(DT, {Key, Item, Item2}).

