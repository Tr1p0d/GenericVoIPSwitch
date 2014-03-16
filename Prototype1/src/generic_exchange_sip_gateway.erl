-module(generic_exchange_sip_gateway).

-behaviour(gen_server).

-export([start_link/0, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_sip/3, transmit_generic/1]).

-include("../include/generic_exchange.hrl").
-include("../deps/nksip/include/nksip.hrl").

route_sip(Msg=#sipmsg{}, _IP, _Port) ->
	gen_server:call(?MODULE, {route_sip_msg, Msg, _IP, _Port}).

transmit_generic(Msg=#generic_msg{}) ->
	gen_server:call(?MODULE, {transmit_generic_msg, Msg}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok,[]}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, State) ->
	lager:warning("~p terminated while because ~p", [?MODULE,  _Reason]),
	ok.

handle_call({route_sip_msg, _Msg=#sipmsg{}, IP, Port}, From,State) ->
	gen_server:reply(From, ok),
	Result = case generic_exchange_dialog_router:
		route_message(generic_exchange_sip_generic:sip_to_generic(_Msg,
			IP, Port)) of
		{ok, transmitted} ->
			{noreply, State};

		_Err -> _Err
	end,
	Result;

handle_call({transmit_generic_msg, {_Msg=#generic_msg{}, IP, Port}}, _From, _State) ->
	generic_exchange_sip_transport:send(
		generic_exchange_sip_generic:generic_to_sip(_Msg), IP, Port),
	{reply, ok, _State};

handle_call(_Msg, _From, _State) ->
	lager:warning('~p received an invalid synchronous message ~p', [?MODULE, _Msg]),
	{noreply, _State}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.

