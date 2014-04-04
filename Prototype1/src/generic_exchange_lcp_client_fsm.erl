-module(generic_exchange_lcp_client_fsm).
-behaviour(gen_fsm).

-export([start_link/0, init/1, code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).

-export([online/3]).

-include("../include/generic_exchange.hrl").

-define(TIMEOUT, 10000).

-record(lcp_client_state, {
		lcd_state				:: list(),
		output_state			:: { speaker | headphone },
		rtp_state				:: list()
	}).


online(Msg, _From, State) ->
	lager:error("", []),
	lager:error("message in lcp_client : ~p", [Msg]),
	lager:error("", []),
	{reply, ok, online, State, ?TIMEOUT}.

start_link() ->
 	gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	{ok, online, #lcp_client_state{lcd_state=[],
			output_state=speaker, rtp_state=[]}, ?TIMEOUT}.	

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
	{reply, StateName, StateName, StateData}.

terminate(_Reason, StateName, _StateData) ->
	generic_exchange_lcp_gateway:remove_lcp_client(self()),
	lager:info("transaction terminated because ~p in state ~p", [_Reason, StateName]),
	normal.
