-module(generic_exchange_dialog_router).
-behaviour(gen_server).

-export([start_link/1, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([route_message/1]).

-include("../include/generic_exchange.hrl").

%% an entry point into generic core
%% should accept any generic message

-spec route_message(#generic_msg{}) -> 
	{ok , transmitted}.

route_message(GenericMSG) ->
 	gen_server:call( ?MODULE, {route_generic_message, GenericMSG}).

-spec start_link( ets:tid() | atom() ) -> 
	{ok, pid()} | {error, term()}.

start_link(DialogETS) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [DialogETS], []).

init([DialogETS]) ->
	{ok,{DialogETS}}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, State) ->
	lager:warning("~p terminated while in ~p", [?MODULE, State]),
	ok.


-spec handle_call({route_generic_message, #generic_msg{}}, term(), {ets:tid() | atom()}) ->
	{noreply, {ets:tid()} }.

handle_call({route_generic_message, GenMSG=#generic_msg{caller=Caller, callee=Callee}}, _From, {DialogETS}) ->

	case {generic_exchange_dialog_manager:lookup_dialog(Caller),
			generic_exchange_dialog_manager:lookup_dialog(Callee)} of
		% dialog found, we will be able to retransmitt our request so respond
		% now
		{{ok, CallerDialog}, {ok, CalleeDialog}} ->
			gen_server:reply(_From, {ok, transmitted}),
			case {gen_fsm:sync_send_event(CallerDialog, {fromTU, GenMSG}),
			gen_fsm:sync_send_event(CalleeDialog, {fromRP, GenMSG})} of

				{ok, ok} -> 
					{ok};

				_Err ->
					{error, not_transmitted}
			end;
		% new dialog lets create a new one
		{{error, not_found}, {error, not_found}} ->
			case {generic_exchange_dialog_manager:create_dialog(_From, Caller),
				generic_exchange_dialog_manager:create_dialog(_From, Callee)} of

				% in case we spawned our dialogs correctly
				{{ok, CallerDialogPID}, {ok, CalleeDialogPID}} ->
					gen_server:reply(_From, {ok, transmitted}),

					case {gen_fsm:sync_send_event(CallerDialogPID, {fromTU, GenMSG}),
					gen_fsm:sync_send_event(CalleeDialogPID, {fromRP, GenMSG})} of

						{ok, ok} -> 
							{ok};

						_Err ->
							{error, not_transmitted}
					end;

				_Err -> 
					_Err
			end
	end,

	{noreply, {DialogETS}}.

-spec handle_cast( term(), term() ) ->
	{noreply, term()}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

-spec handle_info( term(), term() ) ->
	{noreply, term()}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.

