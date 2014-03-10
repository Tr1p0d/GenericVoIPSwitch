-module(generic_dialog_fsm).
-behaviour(gen_fsm).

-export([start_link/1, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([idle/3, ringing/3, dialed/3]).

-include("../include/generic_exchange.hrl").


idle({fromTU, _MSG=#generic_msg{type=make_call}}, _From, _Destination) ->
	{reply, ok, dialed, _Destination};

idle({fromRP, _MSG=#generic_msg{type=make_call}}, _From, _Destination) ->
	{reply, ok, ringing, _Destination}.

dialed(_Msg, _From, _Dest) ->
	ok.

ringing(_Msg, _From, _Dest) ->
	ok.

%ringback() ->
%
%incall() ->
%	
%teardown() ->

start_link(Destination) ->
	gen_fsm:start_link(?MODULE, [Destination], []).

init(Destination) ->
	{ok, idle, Destination}.	

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
	{reply, ok, StateName, StateData}.

terminate(_Reason, StateName, _StateData) ->
	lager:warning("?MODULE terminatedi while in ~p", [StateName]),
	ok.


