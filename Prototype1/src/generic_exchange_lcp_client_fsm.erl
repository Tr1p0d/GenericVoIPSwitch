-module(generic_exchange_lcp_client_fsm).
-behaviour(gen_fsm).

-export([start_link/3, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([available/3, online/3, associating/3, ringback/3]).

-include("../include/generic_exchange.hrl").
%-include("../deps/elcpcp/src/msg_type.hrl").


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

-include("deps/elcpcp/src/msg_type.hrl").


available({transport_cookie_ind, <<"cookie">>}, _Frm, State=#lcp_client_state{
	ip=IP,
	port=Port}) ->

	Gen_msg=create_generic_message(State, associate),
	elcpcp:send_message({IP, Port}, #datagram_cmd{protocol=cornet_msg, msg={led_set_cmd, 0, 1}}),
	{reply, {route, Gen_msg}, associating, State, ?TIMEOUT}.

associating(#generic_msg{type = accept}, _From, State) ->
	lager:warning("client lcp associated"),
	{reply, do_nothing, online, State};

associating(timeout, _From, State) ->
	{stop, normal, State}.

online(#datagram_ind{protocol=cornet_msg, msg=#kbd_down_ind{key=_KEY}}, _From, State) ->
	NewState = State#lcp_client_state{keyboard_state="alice"},
	{reply, {route, create_generic_message(NewState,make_call)}, online, NewState};

online(#generic_msg{type=ring}, _From, State=#lcp_client_state{
		ip=IP,
		port=Port}) ->

	elcpcp:send_message({IP, Port}, #datagram_cmd{protocol=cornet_msg, msg={led_set_cmd, 0, 1}}),

	{reply, do_nothing, ringback, State}. 

ringback(#generic_msg{type=accept}, _From, State) ->
	lager:warning("accepted in call "),
	{reply, do_nothing, incall, State}. 

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
	lager:info("lcp transaction terminated because ~p in state ~p", [_Reason, StateName]),
	generic_exchange_lcp_gateway:remove_lcp_client(self()),
	normal.

-spec create_generic_message(#lcp_client_state{}, term()) ->
	#generic_msg{}.

create_generic_message(#lcp_client_state{
	keyboard_state=KBD,
	identifier=Client_identifier,
	ip=IP,
	port=Port}, associate) ->

	Dialog = nksip_lib:uid(),

	#generic_msg{      
	type			 =associate,
	target 		  	 =list_to_binary(KBD),
	caller 		  	 ={list_to_binary(Client_identifier), Dialog, nksip_lib:uid()},
	callee 		  	 ={list_to_binary(Client_identifier), Dialog, <<>>},
	upstreamRoute  	 = [],
	downstreamRoute  =[],
	routeToRecord 	 =[],
	sequenceNum	  	 = {1, 'REGISTER'},
	specificProtocol =[],
	timeToLive 		 =70,
	receivedOn       = {IP,Port}};

create_generic_message(#lcp_client_state{
	keyboard_state=KBD,
	identifier=Client_identifier,
	ip=IP,
	port=Port}, Method) ->

	Dialog = nksip_lib:uid(),

	#generic_msg{      
	type			 =Method,
	target 		  	 =list_to_binary(KBD),
	caller 		  	 ={list_to_binary(Client_identifier), Dialog, nksip_lib:uid()},
	callee 		  	 ={list_to_binary(KBD), Dialog, <<>>},
	upstreamRoute  	 = [],
	downstreamRoute  =[],
	routeToRecord 	 =[],
	sequenceNum	  	 ={1, 'INVITE'},
	specificProtocol =nksip_sdp:new(),
	timeToLive 		 =70,
	receivedOn       = {IP,Port}}.

