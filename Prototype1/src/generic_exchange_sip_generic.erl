-module(generic_exchange_sip_generic).
-export([sip_to_generic/3, generic_to_sip/1]).

-include("../include/generic_exchange.hrl").
-include("../deps/nksip/include/nksip.hrl").

sip_to_generic(Msg=#sipmsg{
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
    cseq_method = SeqMethod,
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
			[{Via#via.domain, Via#via.port, Via#via.opts}|Acc]
		end,
		[], Vias),
	routeToRecord = [],
	sequenceNum	= {SeqNum, SeqMethod},
	timeToLive=TTL,
	receivedOn={IP,Port},
	specificProtocol = add_specific(Msg)
};


sip_to_generic(Msg=#sipmsg{
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
    cseq_method = SeqMethod,
    forwards = TTL,
    routes = Route,
    from_tag = FromTag,
	to_tag  = ToTag},IP, Port) ->

	[FirstVia | RestVias] = Vias,
	
#generic_msg{
	type = sip_class_to_generic_type(Class), 
	target = Target,
	caller = {FromUser, CallID, FromTag},
	callee = {ToUser, CallID, ToTag},		  
	upstreamRoute = Route,
	downstreamRoute = 	case FirstVia#via.port  of
		 0 ->
			[{FirstVia#via.domain, Port, FirstVia#via.opts}| lists:foldr( 
			fun(Via, Acc) ->
				[{Via#via.domain, Via#via.port, Via#via.opts}|Acc]
			end,
			[], RestVias)];
			 
		_Else ->
			lists:foldr( 
			fun(Via, Acc) ->
				[{Via#via.domain, Via#via.port, Via#via.opts}|Acc]
			end,
			[], Vias)
	end,
	routeToRecord = [],
	sequenceNum	= {SeqNum, SeqMethod},
	timeToLive=TTL,
	receivedOn={IP,Port},
	specificProtocol = add_specific(Msg)}.


% ROUTING RESPONSES
generic_to_sip(GenMsg=#generic_msg{
	type = Type,
	target = _GenTarget,
	callee = {GenFromUser, GenCallID, GenFromTag},
	caller = {GenToUser, GenCallID, GenToTag},		  
	upstreamRoute = UpstreamRoute,
	downstreamRoute = DownstreamRoute,
	routeToRecord = [],
	sequenceNum	= {SeqNum, SeqMethod},
	timeToLive=GenTTL,
	specificProtocol = _SpecMsg}) when Type == ring; Type == accept;
		Type == reject
	->


	#sipmsg{
    id = <<"cgwmoVbhMTt">>,
    class = generic_type_to_sip_class(Type, GenMsg),
    app_id = 0,
    dialog_id = undefined,
	ruri = undefined,
	vias=lists:reverse(lists:map(
		fun({Domain, Port, Opts}) ->
			#via{domain=Domain, port=Port, opts=Opts}
		end,
		DownstreamRoute)),
	from=#uri{
			user=GenFromUser,
			domain = generic_exchange_networking:get_domain(),
			ext_opts=[fill_tag(GenFromTag)]
			
		},
	to=#uri{
			user=GenToUser,
			domain = generic_exchange_networking:get_domain(),
			ext_opts=[fill_tag(GenToTag)]
		},
    call_id = GenCallID,
    cseq = SeqNum,
    cseq_method = SeqMethod,
    forwards = GenTTL,
	routes = UpstreamRoute,
	contacts = [#uri{
		user=GenToUser,
		domain=generic_exchange_networking:get_self_ip(),
		port=generic_exchange_networking:get_port()
	}],
    content_type = resolve_content(GenMsg),
	require = [],
	supported = [],
	expires = undefined,
    event = undefined, 
	headers= [{<<"Expires">>, 3600}],
	body = extract_body_from_generic(GenMsg),
    from_tag = GenFromTag,
    to_tag = GenToTag,
    to_tag_candidate = <<>>,
	transport = #transport{},
    start = 123456789,
	meta = []};

generic_to_sip(GenMsg=#generic_msg{
	type = Type,
	target = GenTarget,
	caller = {GenFromUser, GenCallID, GenFromTag},
	callee = {GenToUser, GenCallID, GenToTag},		  
	upstreamRoute = UpstreamRoute,
	downstreamRoute = DownstreamRoute,
	routeToRecord = [],
	sequenceNum	= {SeqNum, SeqMethod},
	timeToLive=GenTTL,
	specificProtocol = _SpecMsg}) when Type == make_call; Type == teardown
	->

	lager:warning("DSR ~p", [DownstreamRoute]),

	[{Domain2, _Port2, _Opts} | _ ] = DownstreamRoute,

	#sipmsg{
    id = <<"cgwmoVbhMTt">>,
    class = generic_type_to_sip_class(Type, GenMsg),
    app_id = 0,
    dialog_id = undefined,
    ruri = #uri{
				user=GenTarget,
				domain=Domain2
			},
	vias=lists:reverse(lists:map(
		fun({Domain, Port, Opts}) ->
			#via{domain=Domain, port=Port, opts=Opts}
		end,
		DownstreamRoute)),
	from=#uri{
			user=GenFromUser,
			domain = generic_exchange_networking:get_domain(),
			ext_opts=[fill_tag(GenFromTag)]
			
		},
	to=#uri{
			user=GenToUser,
			domain = generic_exchange_networking:get_domain(),
			ext_opts=[fill_tag(GenToTag)]
		},
    call_id = GenCallID,
    cseq = SeqNum,
    cseq_method = SeqMethod,
    forwards = GenTTL,
	routes = UpstreamRoute,
	contacts = [#uri{
		user=GenFromUser,
		domain=generic_exchange_networking:get_self_ip(),
		port=generic_exchange_networking:get_port()
	}],
    content_type = resolve_content(GenMsg),
	require = [],
	supported = [],
	expires = undefined,
    event = undefined, 
	headers= [],
	body = extract_body_from_generic(GenMsg),
    from_tag = GenFromTag,
    to_tag = GenToTag,
    to_tag_candidate = <<>>,
	transport = #transport{},
    start = 123456789,
	meta = []}.

	
%
% 	HELPER FUNCTIONS
%

fill_tag(Tag) ->
	case Tag of
		<<>> ->
			undefined;
		Tag ->
			{<<"tag">>, Tag}
	end.


-spec add_specific(#sipmsg{}) ->
	term().

add_specific(#sipmsg{body=Body}) ->
	case nksip_sdp:is_sdp(Body) of
		true ->
			Body;
		false ->
			[]
	end.

-spec resolve_content(#generic_msg{}) ->
	term().

resolve_content(#generic_msg{specificProtocol=Proto}) ->
	case Proto of
		#sdp{} ->
			{<<"application/sdp">>,[]};
		_ ->
			undefined
	end.

-spec extract_body_from_generic(#generic_msg{}) ->
	term().

extract_body_from_generic(#generic_msg{specificProtocol=Proto}) ->
	case Proto of
		#sdp{} ->
			Proto;
		_ ->
			<<>>
	end.



sip_class_to_generic_type({req, Method})->
	case Method of 
		'INVITE' -> make_call;
		'REGISTER' -> associate;
		'BYE'	  -> teardown;
		'CANCEL'  -> teardown
	end;

sip_class_to_generic_type({resp, Code, _})->
	case Code of 
		180 -> ring;
		200 -> accept;
		603 -> reject
	end.

generic_type_to_sip_class(Method, 
	#generic_msg{sequenceNum={_, ContextMethod}}) ->
	
	case Method of
		accept -> {resp, 200, "OK"};
		ring -> {resp, 180, "RINGING"};
		reject -> {resp, 603, "REJECTED"};

		make_call -> {req, 'INVITE'};
		teardown -> 
			case ContextMethod of
				'CANCEL' ->
					{req, 'CANCEL'};
				'BYE' ->
					{req, 'BYE'}
			end
	end.
