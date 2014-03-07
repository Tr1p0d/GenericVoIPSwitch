
-type generic_msg_method() ::
	make_call | teardown | ring | accept.

%this information is needed not to loose any information during translati
%on process
-type generic_client_identifier() ::
	binary().

-type generic_dialog_identifier() ::
	binary().

-type generic_part_identifier() ::
	binary().

-type generic_dialog_party_identifier() ::
	{ generic_client_identifier(), generic_dialog_identifier(), generic_part_identifier() }.

-type ip_address() ::
	binary().

-type route() ::
	{binary()} | {binary(), ip_address()}.

-type route_path() ::
	[route()].

-record(generic_msg, {      
	type 		  :: generic_msg_method(),
	target 		  :: generic_client_identifier(),
	caller 		  :: generic_dialog_party_identifier(),
	callee 		  :: generic_dialog_party_identifier(),
	upstreamRoute  	  :: route_path(),
	downstreamRoute   :: route_path(),
	routeToRecord 	  :: route_path(),
	sequenceNum	  :: integer() ,
	specificProtocol  :: any()
}).

