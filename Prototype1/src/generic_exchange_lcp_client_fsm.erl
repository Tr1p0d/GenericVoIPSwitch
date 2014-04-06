-module(generic_exchange_lcp_client_fsm).
-behaviour(gen_fsm).

-export([start_link/3, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([available/3, online/3, associating/3]).

-include("../include/generic_exchange.hrl").

-define(TIMEOUT, 10000).

-record(lcp_client_state, {
		lcd_state				:: list(),
		output_state			:: { speaker | headphone },
		rtp_state				:: list(),
		keyboard_state			:: list(),
		identifier				:: integer(),
		ip						:: inet:ip_address(),
		port					:: inet:network_port(),
		active_dialog			:: binary()
	}).



available({transport_cookie_ind, <<"cookie">>}, _Frm, State) ->
	Gen_msg=create_generic_message(State),
	{reply, {route, Gen_msg}
		, next_state, associating, ?TIMEOUT}.

associating(#generic_msg{type = accept}, _From, State) ->
	ok.


online(Msg, _From, State) ->
	ok.

start_link(IP, Port, Ident) ->
 	gen_fsm:start_link(?MODULE, [IP, Port, Ident], []).


init([IP, Port, Ident]) ->
	{ok, available, #lcp_client_state{lcd_state=[],
			output_state=speaker,
			rtp_state=[],
			ip=IP,
			keyboard_state=[],
			port=Port,
			identifier=Ident}, ?TIMEOUT}.	

code_change(_Old, StateName, StateData, _Extra)->
	{ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
	lager:warning('received all state event'),
	{next_state, StateName, StateData}.

handle_info(_Event, StateName, StateData)->
	lager:warning('received unidentified message ~p while in state ~p', [_Event, StateName]),
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	lager:warning('received all sync state event'),
	{reply, StateName, StateName, StateData}.

terminate(_Reason, StateName, _StateData) ->
	generic_exchange_lcp_gateway:remove_lcp_client(self()),
	lager:info("transaction terminated because ~p in state ~p", [_Reason, StateName]),
	normal.

-spec create_generic_message(#lcp_client_state{}) ->
	#generic_msg{}.

create_generic_message(#lcp_client_state{
	keyboard_state=KBD,
	identifier=Client_identifier,
	ip=IP,
	port=Port}) ->

	Dialog = nksip_lib:uid(),

	#generic_msg{      
	type			 =associate,
	target 		  	 =list_to_binary(KBD),
	caller 		  	 ={list_to_binary(Client_identifier), Dialog, nksip_lib:uid()},
	callee 		  	 ={list_to_binary(KBD), Dialog, <<>>},
	upstreamRoute  	 = <<>>,
	downstreamRoute  =[],
	routeToRecord 	 =[],
	sequenceNum	  	 =1 ,
	specificProtocol =[nksip_sdp:new()],
	timeToLive 		 =70,
	receivedOn       = {IP,Port}}.


