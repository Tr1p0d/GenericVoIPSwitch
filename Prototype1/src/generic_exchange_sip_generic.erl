-module(generic_exchange_sip_generic).
-export([sip_to_generic/3, generic_to_sip/1]).

-include("../include/generic_exchange.hrl").
-include("../deps/nksip/include/nksip.hrl").

sip_to_generic(_Msg=#sipmsg{
    class = {resp, _, _}=Class,
    vias = Vias,
    from = #uri{
		user=FromUser
		},
    to  = #uri{
		user=ToUser
		},
    call_id = CallID,
    cseq = SeqNum,
    cseq_method = _SeqMethod,
    forwards = TTL,
    routes = Route,
    from_tag = FromTag,
	to_tag  = ToTag},IP, Port) ->
	
#generic_msg{
	type = sip_class_to_generic_type(Class), 
	target = undefined,
	% switch those two !!!
	callee = {FromUser, CallID, FromTag},
	caller = {ToUser, CallID, ToTag},		  
	upstreamRoute = Route,
	downstreamRoute = lists:foldr( 
		fun(Via, Acc) ->
			[{Via#via.domain, Via#via.port}|Acc]
		end,
		[], Vias),
	routeToRecord = [],
	sequenceNum	= SeqNum,
	timeToLive=TTL,
	receivedOn={IP,Port},
	specificProtocol = _Msg};


sip_to_generic(_Msg=#sipmsg{
    class = Class,
    ruri = #uri{
		user=Target
		},
    vias = Vias,
    from = #uri{
		user=FromUser
		},
    to  = #uri{
		user=ToUser
		},
    call_id = CallID,
    cseq = SeqNum,
    cseq_method = _SeqMethod,
    forwards = TTL,
    routes = Route,
    from_tag = FromTag,
	to_tag  = ToTag},IP, Port) ->
	
#generic_msg{
	type = sip_class_to_generic_type(Class), 
	target = Target,
	caller = {FromUser, CallID, FromTag},
	callee = {ToUser, CallID, ToTag},		  
	upstreamRoute = Route,
	downstreamRoute = lists:foldr( 
		fun(Via, Acc) ->
			[{Via#via.domain, Via#via.port}|Acc]
		end,
		[], Vias),
	routeToRecord = [],
	sequenceNum	= SeqNum,
	timeToLive=TTL,
	receivedOn={IP,Port},
	specificProtocol = _Msg}.

generic_to_sip(GenMsg=#generic_msg{
	type = accept,
	target = GenTarget,
	% switch them back
	callee = {GenFromUser, GenCallID, GenFromTag},
	caller = {GenToUser, GenCallID, GenToTag},		  
	upstreamRoute = UpstreamRoute,
	downstreamRoute = DownstreamRoute,
	routeToRecord = [],
	sequenceNum	= SeqNum,
	timeToLive=GenTTL,
	specificProtocol = SpecMsg}) ->

	#sipmsg{
    ruri = SIPTarget,
    vias = Vias,
    from = SipFrom,
    to  = SipTo
	}=SpecMsg,

	SpecMsg#sipmsg{
		class = {resp, 200, "OK"},
		ruri = undefined,
		vias = Vias,
		from=SipFrom#uri{
				user=GenFromUser
			},
		to=SipTo#uri{
				user=GenToUser
			},
		cseq=SeqNum,
		forwards=GenTTL
		};

generic_to_sip(GenMsg=#generic_msg{
	type = ring,
	target = GenTarget,
	% switch them back
	callee = {GenFromUser, GenCallID, GenFromTag},
	caller = {GenToUser, GenCallID, GenToTag},		  
	upstreamRoute = UpstreamRoute,
	downstreamRoute = DownstreamRoute,
	routeToRecord = [],
	sequenceNum	= SeqNum,
	timeToLive=GenTTL,
	specificProtocol = SpecMsg}) ->

	#sipmsg{
    ruri = SIPTarget,
    vias = Vias,
    from = SipFrom,
    to  = SipTo
	}=SpecMsg,

	SpecMsg#sipmsg{
		class = {resp, 180, "RINGING"},
		ruri = undefined,
		vias = Vias,
		from=SipFrom#uri{
				user=GenFromUser
			},
		to=SipTo#uri{
				user=GenToUser
			},
		cseq=SeqNum,
		forwards=GenTTL
		};

generic_to_sip(GenMsg=#generic_msg{
	type = Type,
	target = GenTarget,
	caller = {GenFromUser, GenCallID, GenFromTag},
	callee = {GenToUser, GenCallID, GenToTag},		  
	upstreamRoute = UpstreamRoute,
	downstreamRoute = DownstreamRoute,
	routeToRecord = [],
	sequenceNum	= SeqNum,
	timeToLive=GenTTL,
	specificProtocol = SpecMsg}) ->

	#sipmsg{
    ruri = SIPTarget,
    vias = Vias,
    from = SipFrom,
    to  = SipTo
	}=SpecMsg,

	lager:info("~p", [DownstreamRoute]),

	[{Domain2, Port2} | DST] = DownstreamRoute,

	SpecMsg#sipmsg{
		class = generic_type_to_sip_class(Type, GenMsg),
		ruri = SIPTarget#uri{
				user=GenTarget
			},
		vias=lists:reverse([#via{
					domain=Domain2,
					port=Port2,
					opts=[{branch, nksip_lib:uid()},rport]}
					|lists:reverse(lists:zipwith(
			fun({Domain, Port}, Via) ->
				Via#via{domain=Domain, port=Port}
			end,
			DST, Vias))]),
		from=SipFrom#uri{
				user=GenFromUser
			},
		to=SipTo#uri{
				user=GenToUser
			},
		cseq=SeqNum,
		forwards=GenTTL
		}.
	
%
% 	HELPER FUNCTIONS
%
sip_class_to_generic_type({req, Method})->
	case Method of 
		'INVITE' -> make_call;
		'REGISTER' -> associate
	end;

sip_class_to_generic_type({resp, Code, _})->
	case Code of 
		180 -> ring;
		200 -> accept
	end.

generic_type_to_sip_class(Method, #generic_msg{}) ->
	case Method of
		accept -> {resp, 200, "OK"};
		ring -> {resp, 180, "RINGING"};

		make_call -> {req, 'INVITE'}
	end.
