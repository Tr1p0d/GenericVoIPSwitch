-module(generic_exchange_dialog_router).
-behaviour(gen_server).

-export([start_link/1, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_message/1]).

-include("../include/generic_exchange.hrl").

%% an entry point into generic core
%% should accept any generic message
route_message(GenericMSG) ->
	gen_server:call({route_generic_message, GenericMSG}).

start_link(DialogAA) ->
	gen_server:start_link({local, generic_exchange_dialog_router}, ?MODULE, [DialogAA], []).

init([DialogAA]) ->
	{ok,DialogAA}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, State) ->
	lager:warning("?MODULE terminatedi while in ~p", [State]),
	ok.

handle_call({route_generic_message, _GenMSG}, _From, _DialogAA) ->
	case dict:find(smt) of
		% dialog found
		{ok, _Value} ->
			ok;
		% new dialog lets create a new one
		error ->
			ok
	end;

handle_call(_Msg, _From, _State) ->
	lager:warning('?MODULE received an invalid synchronous message ~p', [_Msg]),
	{noreply, _State}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.
