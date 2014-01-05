-module(generic_switch_sip_router).
-behaviour(gen_server).

-export([start_link/1, code_change/3, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).

-export([async_incomming_message/1]).

-include("../deps/nksip/include/nksip.hrl").

-record(dialogRouterState, 
	{
		dialogTable
	}
).

-record(sipDialog,
	{
		callID,
		toTag,
		fromTag
	}
).

% receives incomming sip messages
async_incomming_message(Msg) ->
	gen_server:cast(?MODULE, {incomming_message, Msg}).

start_link(DialogTable) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {DialogTable}, []).

init({DialogTable}) ->
	lager:info("sip dialog router had started"),
	{ok, #dialogRouterState{dialogTable=DialogTable}}.

handle_cast({incomming_message, Msg}, State) -> 
	#sipmsg{
		id = ID,
		class =Class,
		app_id = _AppID             ,
		dialog_id = _dialog_id      ,
		ruri = Ruri                 ,
		vias = Vias                 ,
		from = From                 ,
		to = To                     ,
		call_id = CallId            ,
		cseq = Cseq                 ,
		cseq_method = _CseqMet      ,
		forwards = _Forwards        ,
		routes = _Routes            ,
		contacts = Contacs          ,
		content_type = Content      ,
		require = _Req              ,
		supported = _Supp           ,
		expires = Expires           ,
		event = Event               ,
		headers = Headers           ,
		body = Body                 ,
		from_tag = FromTag          ,
		to_tag = ToTag              ,
		to_tag_candidate = Candidate, 
		transport = _Trans,
		start = Start
	}=Msg,

	lager:info(" ID ~p~n Class ~p~n Ruri ~p~n Vias ~p~n Event ~p~n Headers ~p~n Candidate ~p~n",
		[ID, Class, Ruri, Vias, Event, Headers, Candidate]),
	{noreply, State}.

handle_call(Request, _From, _State) ->
	lager:warning("unexpected message received ~p", [Request]),
	{noreply, _State}.

handle_info(Request, _State) ->
	lager:warning("unexpected message received ~p", [Request]),
	{noreply, _State}.

terminate(_Reason, _State) ->
	lager:warning("shutting down the UDP transport ~p", [_Reason]),
	ok.

code_change(_Old, _State, _Extra) ->
	lager:warning("hotcode swap not supported").

resolve


