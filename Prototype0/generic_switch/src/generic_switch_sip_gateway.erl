-module(generic_switch_sip_gateway).
-behaviour(gen_server).

-export([start_link/0, terminate/2, init/1, handle_call/3, handle_cast/2,
		code_change/3, handle_info/2]).
-export([async_register/1]).

-include("../deps/nksip/include/nksip.hrl").

start_link() ->
	lager:info("generic switch sip gateway had started"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	{ok, []}.

% register new client, call router do not know how to route this request
async_register(DialogID) ->
	gen_server:cast(?MODULE, {register, {unknown_client, DialogID}}).

code_change( _, _, _) ->
	{ok}.

handle_call(Message, _from, State) ->
	lager:info("unexpected synchronous message ~p", [Message]),
	{noreply, State}.

%% OUTGOING SIP MESSAGE
handle_cast({register, {unknown_client, DialogID}}, State) ->
	Client = client_database:addClient({?MODULE, DialogID}),
	gen_server:cast(?MODULE, {client_created, {DialogID, Client}}),
	{noreply, State};

%% INCOMMING GENERIC MESSAGES
handle_cast({client_created, {DialogID, Client}}, State) ->

	SipMsg=#sipmsg{
		class = {resp, 200, <<"OK">>}
	},

	generic_switch_sip_router:
	async_outgoing_sip_message(SipMsg, DialogID, Client),
	{noreply, State};

handle_cast(Message, State) ->
	lager:info("unexpected asynchronous message ~p", [Message]),
	{noreply, State}.

handle_info(Message, State) ->
	lager:info("unexpected asynchronous message ~p", [Message]),
	{noreply, State}.

terminate(Reason, _State) ->
	lager:warning("generic switch sip gateway terminating  : ~p", [Reason]).

