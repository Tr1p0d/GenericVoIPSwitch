-module(generic_exchange_transport_sip_udp).
-behaviour(gen_server).

-include("../deps/nksip/include/nksip.hrl").

-record(udpstate, 
	{
		socket
	}
).

%% API

send(SipMsg, Ip ,Port) ->
	gen_server:cast(?MODULE, {send,{nksip_unparse:packet(SipMsg), Ip, Port}}).

start_link(GenUDPSpecs) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, GenUDPSpecs, []).


init({Port, Specs}) ->
	%case gen_udp:open(5060, [binary, {active, false}]) of 
	case gen_udp:open(Port, Specs) of 
		{ok, Socket} ->
			lager:info("sip udp transport had started"),
			{ok, #udpstate
				{
					socket=Socket
				}
			};
		{error, _ } ->
			{stop, cannot_bind}
	end.

code_change(_Old, _State, _Extra) ->
	lager:warning("hotcode swap not supported").

handle_cast({send, {Packet, Ip, Port}}, State=#udpstate{socket=Sock}) ->
	lager:info("message send : ~p ~p", [Ip, Port]),
	gen_udp:send(Sock, binary_to_list(Ip), Port, Packet),
	{noreply, State};

handle_cast(_Request, _State) -> 
	lager:warning("asynchronous calls not supported").

%% a packet was received
handle_info({udp, Socket, IP, Port, Packet}, State=#udpstate{socket=Socket}) ->
	case nksip_parse:packet(0, #transport{proto=0}, Packet) of
		{ok, RawSipMsg, _More} ->
			case nksip_parse:raw_sipmsg(RawSipMsg) of
				Msg=#sipmsg{} ->
					% sends our sip message to dialog router for further
					% processing
					generic_switch_sip_router:
					async_incomming_sip_message(Msg, IP, Port);
				{error, Code, _} -> 
					lager:warning("response code ~p" , [Code]);
				{error, _ } -> 
					lager:warning("cannot parse message")
			end;
		{rnrn, _More} ->
			lager:warning("NOT IMPLEMENTED rnrn");
		{more, _More} ->
			lager:warning("NOT IMPLEMENTED more")
	end,
	{noreply, State};

handle_info(Request, _State) -> 
	lager:notice("unknown message received ~p", [Request]),
	{noreply, _State}.

terminate(_Reason, #udpstate{socket=Socket}) ->
	gen_udp:close(Socket),
	lager:warning("shutting down the UDP transport").
