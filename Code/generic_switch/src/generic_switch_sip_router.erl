-module(generic_switch_sip_router).
-behaviour(gen_server).

-export([start_link/1, code_change/3, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).

-export([async_incomming_sip_message/3, lookup/3, associate/4]).

-include("../deps/nksip/include/nksip.hrl").

-record(dialogRouterState, 
	{
		dialogTable
	}
).

% receives incomming sip messages
async_incomming_sip_message(Msg, IP, Port) ->
	gen_server:cast(?MODULE, {incomming_message, Msg, IP, Port}).

start_link(DialogTable) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {DialogTable}, []).

init({DialogTable}) ->
	lager:info("sip dialog router had started"),
	{ok, #dialogRouterState{dialogTable=DialogTable}}.

handle_cast({incomming_message, Msg, _IP, _Port}, 
	State=#dialogRouterState{dialogTable=ClientMapTable}) -> 
	#sipmsg{
		class = MessageClass,
		vias = [ #via{proto=_ViaProto,
			domain=ViaDomain,
			port=ViaPort,
			opts=_ViaOpts} | _Vias ]	
	}=Msg,

	% lookup source

	case lookup(ViaDomain, ViaPort, ClientMapTable)	of
		[] -> % not found
			case MessageClass of
				% in case of the first register
				{req, 'REGISTER'} ->
					case generic_switch_sip_gateway:register() of
						{ok, Client} ->
							lager:notice("Client ~p associate with transport ~p ~p",
								[Client, ViaPort, ViaDomain]),
							associate(ClientMapTable, ViaDomain,
								ViaPort, Client);	

						{already_started, Client} ->
							lager:warning("request for already active client"),
							associate(ClientMapTable, ViaDomain,
								ViaPort, Client);	

						{error, Reason} ->
							lager:warning("cannot register : ~p", [Reason])
					end;
				{req, _Method} ->
					% constroct a not found message
					lager:info("request");
				{resp, _Code, _binary } ->
					lager:info("response")
			end
	end,	
	{noreply, State}.

handle_call(Request, _From, _State) ->
	lager:warning("unexpected message received ~p", [Request]),
	{noreply, _State}.

handle_info(Request, _State) ->
	lager:warning("unexpected message received ~p", [Request]),
	{noreply, _State}.

terminate(_Reason, _State) ->
	lager:warning("shutting down the SIP router ~p", [_Reason]),
	ok.

code_change(_Old, _State, _Extra) ->
	lager:warning("hotcode swap not supported").

%% checkout wheter we know how to route this information
lookup(Domain, Port, Table) ->
	ets:match(Table, {Domain, Port, $0}).

associate(Table, Domain, Port, Client) ->
	ets:insert(Table, {Domain, Port, Client}).

