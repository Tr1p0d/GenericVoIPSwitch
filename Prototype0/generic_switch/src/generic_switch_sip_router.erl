-module(generic_switch_sip_router).
-behaviour(gen_server).

-export([start_link/1, code_change/3, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).

-export([async_incomming_sip_message/3, lookupDialog/2, createDialog/3]).
-export([async_outgoing_sip_message/3]).

-include("../deps/nksip/include/nksip.hrl").

-record(dialogRouterState, 
	{
		dialogTable
	}
).

-record(sipDialog,
	{
		ip,
		port,
		to,
		from,
		cseq,
		transactions
	}
).

async_outgoing_sip_message(Msg, DialogID, Client) ->
	gen_server:cast(?MODULE, {Msg, DialogID, Client}).
	

% receives incomming sip messages
async_incomming_sip_message(Msg, IP, Port) ->
	gen_server:cast(?MODULE, {incomming_message, Msg, IP, Port}).

start_link(DialogTable) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {DialogTable}, []).

init({DialogTable}) ->
	lager:info("sip dialog router had started with dialog table ~p",
		[DialogTable]),
	{ok, #dialogRouterState{dialogTable=DialogTable}}.

%% ------------------------------------
%% OUTGOIND SIP MESSAGES
%% ------------------------------------

handle_cast({Msg, DialogID, _Client},
	State=#dialogRouterState{dialogTable=DT}) ->

	[[#sipDialog{
			ip=ViaDomain,
			port=ViaPort,
			from=From,
			to=To,
			cseq=Cseq,
			transactions=ViaOpts
		}]]	= lookupDialog(DT, DialogID),

	#sipmsg{
		class=Class
	}=Msg,

	SipMsg=#sipmsg{
		class=Class,
		from=From,
		to=append_to_tag(To),
		cseq=Cseq,
		cseq_method= 'REGISTER',
		call_id=DialogID,
		vias=ViaOpts,
		expires=60
	},



	generic_switch_sip_transport_udp:send(SipMsg, ViaDomain, ViaPort),
	{noreply, State};
	

%% ------------------------------------
%% INCOMMING SIP MESSAGES
%% ------------------------------------

handle_cast({incomming_message, Msg, _IP, _Port}, 
	State=#dialogRouterState{dialogTable=ClientMapTable}) -> 
	#sipmsg{
		class = MessageClass,
		vias = Via,
			from=From,
			to=To,
			cseq=Cseq,
			call_id=CallID
	}=Msg,
	
	[ #via{
			domain=ViaDomain,
			port=ViaPort,
			opts= _ViaOpts } | []] = Via,

	case lookupDialog(ClientMapTable, CallID)	of
		[] -> % not found

			Dialog=#sipDialog{
			ip=ViaDomain,
			port=ViaPort,
			from=From,
			to=To,
			cseq=Cseq,
			transactions=Via},

			createDialog(ClientMapTable, CallID, Dialog),

			case MessageClass of
				% in case of the first register
				{req, 'REGISTER'} ->
					generic_switch_sip_gateway:
						async_register(CallID);
				{req, _Method} ->
					% constroct a not found message
					lager:info("request");
				{resp, _Code, _binary } ->
					lager:info("response")
			end;
		[Dialog] ->
			lager:warning("dialog found!!! ~p", Dialog)
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

lookupDialog(Table, CallID) ->
	ets:match(Table, {CallID, '$1'}).

createDialog(Table, CallID, Dialog) ->
	ets:insert(Table, {CallID, Dialog}).

create_to_tag() ->
	<<"12345sdfgh">>.

append_to_tag(#uri{
		disp=Disp,
		scheme=Scheme,
		user=User,
		pass=Pass,
		domain=Domain,
		port=Port,
		opts=Opts,
		headers=Headers,
		ext_opts=ExtOpts,
		ext_headers=ExtHeaders
	}) ->
	
	#uri{
		disp=Disp,
		scheme=Scheme,
		user=User,
		pass=Pass,
		domain=Domain,
		port=Port,
		opts=Opts,
		headers=Headers,
		ext_opts= [{<<"tag">>, create_to_tag()} | ExtOpts],
		ext_headers=ExtHeaders
	}.












