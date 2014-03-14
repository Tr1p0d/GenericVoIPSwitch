-module(generic_exchange_dialog_router).
-behaviour(gen_server).

-export([start_link/2, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_message/1, lookup_dialogs/3]).

-include("../include/generic_exchange.hrl").

%% an entry point into generic core
%% should accept any generic message
route_message(GenericMSG) ->
	gen_server:call( ?MODULE, {route_generic_message, GenericMSG}).

start_link(DialogETS, AssociationAA) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [DialogETS, AssociationAA], []).

init([DialogETS, AssociationAA]) ->
	{ok,{DialogETS, AssociationAA}}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, State) ->
	lager:warning("?MODULE terminatedi while in ~p", [State]),
	ok.

handle_call({route_generic_message, GenMSG=#generic_msg{caller=Caller, callee=Callee}}, _From, {DialogETS, ASsociationAA}) ->

	Result = case lookup_dialogs(Caller, Callee, DialogETS) of
		% dialog found
		{ok, {_CallerPart, CallerDialog}, {_CalleePart, CalleeDialog}} ->
			case {gen_fsm:sync_send_event(CallerDialog, {fromTU, GenMSG}),
			gen_fsm:sync_send_event(CalleeDialog, {fromRP, GenMSG})} of

				{ok, ok} -> 
					{ok, transmitted};

				_Err ->
					{error, not_transmitted}
			end;
		% new dialog lets create a new one
		{error, not_found} ->
			{_,DialogID, CallerPartID} = Caller,
			{_,DialogID, CalleePartID} = Callee,
			
			case {generic_dialog_fsm:start_link(_From),
			generic_dialog_fsm:start_link(_From)} of

				% in case we spawned our dialogs correctly
				{{ok, CallerDialogPID}, {ok, CalleeDialogPID}} ->
				  	add_dialog(DialogID, CallerPartID, CallerDialogPID, DialogETS),
				  	add_dialog(DialogID, CalleePartID, CalleeDialogPID, DialogETS),

					case {gen_fsm:sync_send_event(CallerDialogPID, {fromTU, GenMSG}),
					gen_fsm:sync_send_event(CalleeDialogPID, {fromRP, GenMSG})} of

						{ok, ok} -> 
							{ok, transmitted};

						_Err ->
							{error, not_transmitted}
					end;

				_Err -> 
					_Err
			end;

		% serious error, discrepancy found
		{error, Error} ->
			{error, Error}
	end,

	{reply, Result, {DialogETS, ASsociationAA}};

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
