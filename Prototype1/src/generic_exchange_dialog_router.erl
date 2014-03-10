-module(generic_exchange_dialog_router).
-behaviour(gen_server).

-export([start_link/1, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_message/1, lookup_dialogs/3]).

-include("../include/generic_exchange.hrl").

%% an entry point into generic core
%% should accept any generic message

route_message(GenericMSG) ->
	gen_server:call( ?MODULE, {route_generic_message, GenericMSG}).

start_link(DialogETS) ->
	gen_server:start_link({local, generic_exchange_dialog_router}, ?MODULE, [DialogETS], []).

init([DialogETS]) ->
	{ok,DialogETS}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, State) ->
	lager:warning("?MODULE terminatedi while in ~p", [State]),
	ok.

handle_call({route_generic_message, _GenMSG=#generic_msg{caller=Caller, callee=Callee}}, _From, DialogETS) ->

	Result = case lookup_dialogs(Caller, Callee, DialogETS) of
		% dialog found
		{ok, {CallerPart, CallerDialog}, {CalleePart, CalleeDialog}} ->
			{ok, routed};

		% new dialog lets create a new one
		{error, not_found} ->
			{_,DialogID, CallerPartID} = Caller,
			{_,DialogID, CalleePartID} = Callee,
			case {add_dialog(DialogID, CallerPartID, <<"uniqueCallerTrans">>, DialogETS) 
				, add_dialog(DialogID, CalleePartID, <<"uniqueCallerTrans2">>, DialogETS) } of
				
				{true, true} ->
					{ok, created};

				_Err -> 
					_Err
			end;
		% serious error, discrepancy found
		{error, Error} ->
			{error, Error}
	end,

	{reply, Result, DialogETS};

handle_call(_Msg, _From, _State) ->
	lager:warning('?MODULE received an invalid synchronous message ~p', [_Msg]),
	{noreply, _State}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.

% ---
% HELPER FNCTIONS
% ---

lookup_dialogs({_CallerClientID, CallerDialogID, CallerPartID}, 
	{_CalleeClientID, _CalleeDialogID, CalleePartID},
	DialogETS) ->
	
	case ets:match(DialogETS, {CallerDialogID, '$1', '$2'}) of
		[[CallerPartID, CallerDialog], [CalleePartID, CalleeDialog]] ->
			{ok, {CallerPartID, CallerDialog}, {CalleePartID, CalleeDialog}};
		[[CalleePartID, CalleeDialog], [CallerPartID, CallerDialog]] ->
			{ok, {CallerPartID, CallerDialog}, {CalleePartID, CalleeDialog}};
		[] ->
			{error, not_found};
		_Error  ->
			lager:warning("ets inconsistency found"),
			{error, _Error}
	end.	

add_dialog(DialogID, CallerPartID, Transaction, DialogETS) ->
	ets:insert(DialogETS, {DialogID, CallerPartID, Transaction}).
