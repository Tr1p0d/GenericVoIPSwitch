-module(generic_exchange_sip_message_factory).
-export([method_not_allowed/1, ack/1]).

-include("../include/generic_exchange.hrl").
-include("../deps/nksip/include/nksip.hrl").

method_not_allowed(Msg=#sipmsg{}) ->
	Msg#sipmsg{
		class = {resp, 405, "Method not allowed"},
		ruri = undefined	
	}.

ack(MSG=#sipmsg{}) ->
	MSG#sipmsg{
	class = {req, 'ACK'},
	ruri = #uri{
		user=MSG#sipmsg.from#uri.user,
		domain=generic_exchange_networking:get_self_ip(),
		port=generic_exchange_networking:get_port()
	},
	%vias= lists:sublist(MSG#sipmsg.vias,length(MSG#sipmsg.vias) -1 ),
    cseq_method = 'ACK',
    forwards = 70}.
	%routes = UpstreamRoute,
	%contacts = [#uri{
	%	%user=GenToUser,
	%	domain=generic_exchange_networking:get_domain(),
	%	port=generic_exchange_networking:get_port()
	%}],
	%content_type = undefined,
	%require = [],
	%supported = [],
	%expires = undefined,
    %event = undefined, 
	%headers= [],
	%body = [],
    %from_tag = GenFromTag,
    %to_tag = GenToTag,
    %to_tag_candidate = <<>>,
	%transport = #transport{},
    %start = 123456789,
	%meta = []}.

