-module(generic_switch_sip_transport_udp).
-export([start_link/1, code_change/3, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).
-export([get_socket/0]).
-behaviour(gen_server).

-include("../deps/nksip/include/nksip.hrl").

-record(udpstate, 
	{
		socket
	}
).

start_link(GenUDPSpecs) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, GenUDPSpecs, []).

get_socket() ->
	gen_server:call(?MODULE, {get_socket}).

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

handle_call({get_socket}, _From, #udpstate{socket=Socket}=State) ->
	{reply, Socket, State}.

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
	lager:warning("shutting down the UDP transport"),
	ok.
	


