
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

-record(generic_msg, {      
	type 		  :: generic_msg_method(),
	target 		  :: generic_client_identifier(),
	caller 		  :: generic_dialog_party_identifier(),
	callee 		  :: generic_dialog_party_identifier(),
	upstreamRoute  	  :: ,
	downstreamRoute   ,
	routeToRecord 	  ,
	sequenceNum	  ,
	specificProtocol  
}).

