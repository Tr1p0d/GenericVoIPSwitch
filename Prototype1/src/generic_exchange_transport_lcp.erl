-module(generic_exchange_transport_lcp).
-export([start/0, on_connect/1, on_message/2, on_disconnect/1]).
-behaviour(elcpcp_listener).


on_connect(Client) ->
    io:format("connect ~p~n", [Client]),
    norepl y.

-spec on_message({inet:ip_address(), inet:network_port()}, term()) ->
	noreply.

on_message({IP, Port}, Msg) ->
	generic_exchange_lcp_gateway:route_lcp(Msg, IP, Port),
    noreply.

on_disconnect(Client) ->
    io:format("disconnect ~p~n", [Client]),
     noreply.

send() ->
	ok.
 

start() -> 
    ok = application:start(elcpcp),
    {ok, _Pid} = elcpcp_listener:create(?MODULE, []).
