-module(generic_exchange_sip_gateway).

-behaviour(gen_server).

-export([start_link/0, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_sip/3, transmit_generic/1]).

-include("../include/generic_exchange.hrl").
-include("../deps/nksip/include/nksip.hrl").



-spec route_sip(#sipmsg{}, inet:ip_address(), inet:port_number()) ->
	ok.

route_sip(Msg=#sipmsg{}, _IP, _Port) ->
	gen_server:call(?MODULE, {route_sip_msg, Msg, _IP, _Port}).

-spec transmit_generic(#generic_msg{}) ->
	ok.

transmit_generic(Msg=#generic_msg{}) ->
	gen_server:call(?MODULE, {transmit_generic_msg, Msg}).

start_link() ->
 	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok,[]}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, _State) ->
	lager:warning("~p terminated while because ~p", [?MODULE,  _Reason]),
	normal.

-spec handle_call({Action, Msg, inet:ip_address(), inet:port_number()}, term(), term()) ->
	{reply, ok, term()} | {noreply, term()}
	when Action :: route_sip_msg | transmit_generic_msg,
		 Msg :: #sipmsg{} | #generic_msg{}.

handle_call({route_sip_msg, _Msg=#sipmsg{class={resp, 100, _}}, _IP, _Port}, _From,State) ->
	{reply, ok, State};

handle_call({route_sip_msg, _Msg=#sipmsg{class={req, 'ACK'}}, _IP, _Port}, _From,State) ->
	{reply, ok, State};

handle_call({route_sip_msg, Msg=#sipmsg{class={req, Method}}, IP, Port},
	From,State) when Method == 'PUBLISH' ->
	gen_fsm:reply(From, ok),
	generic_exchange_transport_sip_udp:send(
		generic_exchange_sip_message_factory:method_not_allowed(Msg), IP, Port),
	
	{noreply, State};

	
handle_call({route_sip_msg, Msg=#sipmsg{}, IP, Port}, From,State) ->
	gen_server:reply(From, ok),
	Result = case generic_exchange_dialog_router:
		route_message(generic_exchange_sip_generic:sip_to_generic(Msg,
			IP, Port)) of
		{ok, transmitted} ->
			{noreply, State}
	end,

	case Msg of
		#sipmsg{class={resp, 200, _}, cseq_method='INVITE'} ->
			handle_specific(generic_exchange_sip_message_factory:ack(Msg), IP, Port);

		#sipmsg{class={resp, 603, _}, cseq_method='INVITE'} ->
			handle_specific(generic_exchange_sip_message_factory:ack(Msg), IP, Port);
		Error ->
			Error
	end,
	Result;

handle_call({transmit_generic_msg, {_Msg=#generic_msg{}, IP, Port}}, _From, _State) ->
	generic_exchange_transport_sip_udp:send(
		generic_exchange_sip_generic:generic_to_sip(_Msg), IP, Port),
	{reply, ok, _State};

handle_call(_Msg, _From, _State) ->
	lager:warning('~p received an invalid synchronous message ~p', [?MODULE, _Msg]),
	{noreply, _State}.


-spec handle_cast(term(), term()) ->
	{noreply, term()}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

-spec handle_info(term(), term()) ->
	{noreply, term()}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.

-spec handle_specific( #sipmsg{} , term(), term()) ->
	ok.

handle_specific( MSG , IP, Port) ->
	generic_exchange_transport_sip_udp:send(MSG, IP, Port),
	ok.





