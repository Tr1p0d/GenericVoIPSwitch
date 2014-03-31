-module(generic_exchange_dialog_manager).
-behaviour(gen_server).

-export([start_link/2, init/1, code_change/3, terminate/2]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

-export([create_dialog/2, lookup_dialog/1, delete_dialog/1]).

-record(dialog_manager_state, {
		dialog_table,
		association_table}).

-include("../include/generic_exchange.hrl").

-spec create_dialog(pid(), generic_dialog_party_identifier()) ->
	{ok, pid()}.

create_dialog(Gateway, Client) ->
	gen_server:call(?MODULE, {create_dialog, Gateway, Client}).

-spec delete_dialog(pid()) ->
	ok.

delete_dialog(PID) ->
	gen_server:call(?MODULE, {delete_dialog, PID}).

-spec lookup_dialog(generic_dialog_party_identifier()) ->
	{ok, pid()}.

lookup_dialog(Client) -> 
	gen_server:call(?MODULE, {lookup_dialog, Client}).

-spec start_link(ets:tid() | atom(), ets:tid() | atom()) ->
	{ok, pid()} | {error, term()}.

start_link( _DT, _AT) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {_DT, _AT}, []).

init({DialogTable, AssociationTable}) ->
	{ok, #dialog_manager_state{
			dialog_table=DialogTable,
			association_table=AssociationTable}}.

code_change(_Old, State, _Extra)->
	{ok, State}.

terminate(_Reason, _State) ->
	lager:warning("~p terminated while because ~p", [?MODULE,  _Reason]),
	ok.

-spec handle_call( Message, {pid(), term()}, #dialog_manager_state{}) ->
	Reply
	when Message :: {create_dialog, pid(), generic_dialog_party_identifier()} |
					{delete_dialog, pid()} |
					{lookup_dialog, generic_dialog_party_identifier()},
					Reply ::   {reply, {ok, pid()}, #dialog_manager_state{}}
					| {reply, ok, #dialog_manager_state{}}
					| {reply, {error, not_found}, #dialog_manager_state{}}.

handle_call({create_dialog, Gateway, {_ClientID, DialogID, PartID}},
	_From, State=#dialog_manager_state{dialog_table=DT, association_table=AT}) ->
	{ok, PID} = supervisor:start_child(generic_exchange_dialog_sup, [Gateway, AT] ),
	ets:insert(DT, {DialogID, PartID, PID}),
	{reply,  {ok, PID}, State};

handle_call({delete_dialog, PID},
	_From, State=#dialog_manager_state{dialog_table=DT, association_table=_AT}) ->
	lager:notice("table before ~p ~p", [ets:match(DT, {'$1', '$2', '$3'}), PID]),
	ets:match_delete(DT, { '_', '_' , PID}),
	lager:notice("table after ~p", [ets:match(DT, {'$1', '$2', '$3'})]),
	{reply,  ok, State};

handle_call({lookup_dialog, {_ClientID, DialogID, PartID}}, _From, 
	State=#dialog_manager_state{dialog_table=DT}) ->
	Result = case ets:match(DT, {DialogID, PartID, '$1'}) of
			[[Dialog]] -> {ok, Dialog};

			[]	-> 
				case ets:match(DT, {DialogID, <<>>, '$2'}) of
					[[Dialog]] -> 
						ets:delete(DT, {DialogID, '<<>>', Dialog}),
						ets:insert(DT, {DialogID, PartID, Dialog}),
						{ok, Dialog};
					[] ->
						{error, not_found}
				end
	end,
	{reply, Result , State}.

handle_cast(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected asynchronous message'),
	{noreply, _State}.

handle_info(_Msg, _State) ->
	lager:warning('?MODULE received an unexpected message'),
	{noreply, _State}.
