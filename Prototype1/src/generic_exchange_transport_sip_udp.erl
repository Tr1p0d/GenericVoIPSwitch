-module(generic_exchange_transport_sip_udp).
-behaviour(gen_server).

-include("../deps/nksip/include/nksip.hrl").

-record(udpstate, 
	{
		socket
	}
).
-export([start_link/1, code_change/3, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).
-export([send/3]).


%% API

-spec send(#sipmsg{}, inet:ip_address(), inet:port_number()) ->
	ok.

send(SipMsg, Ip ,Port) ->
	gen_server:cast(?MODULE, {send,{nksip_unparse:packet(SipMsg), Ip, Port}}).


-spec start_link({term()}) -> 
	{ok, pid} | {error, Error}
	when Error :: {already_started, pid()}.


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

-spec code_change(term() , term(), term()) -> 
	ok.

code_change(_Old, _State, _Extra) ->
	lager:warning("hotcode swap not supported"),
	ok.

-spec handle_call(term(), term(), term()) ->
	{noreply, term()}.

handle_call(_Msg, _From, _State) ->
	lager:warning("synchronous calls are not supported at all"),
	{noreply, _State}.

-spec handle_cast({send, {term(), inet:ip_address(), inet:port_number()}}, #udpstate{}) ->
	{noreply, term()}.

handle_cast({send, {Packet, Ip, Port}}, State=#udpstate{socket=Sock}) ->
	lager:info("message send : ~p ~p", [Ip, Port]),
	gen_udp:send(Sock, {127,0,0,1}, Port, Packet),
	{noreply, State};

handle_cast(_Request, _State) -> 
	lager:warning("asynchronous calls not supported").

-spec handle_info({udp, term(), inet:ip_address(), inet:port_number(), term()}, #udpstate{}) ->
	{noreply, term()}.

%% a packet was received
handle_info({udp, Socket, IP, Port, Packet}, State=#udpstate{socket=Socket}) ->
	case nksip_parse:packet(0, #transport{proto=0}, Packet) of
		{ok, RawSipMsg, _More} ->
			case nksip_parse:raw_sipmsg(RawSipMsg) of
				Msg=#sipmsg{} ->
					% sends our sip message to dialog router for further
					% processing
					generic_exchange_sip_gateway:
					route_sip(Msg, IP, Port)
			end
	end,
	{noreply, State};

handle_info(Request, _State) -> 
	lager:notice("unknown message received ~p", [Request]),
	{noreply, _State}.

-spec terminate(term(), #udpstate{}) ->
	normal.

terminate(_Reason, #udpstate{socket=Socket}) ->
	lager:warning("shutting down the SIP UDP transport : ~p", [_Reason]),
	ok.
