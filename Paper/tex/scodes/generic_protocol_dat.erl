-record(generic_msg, {      
	type 		  :: generic_msg_method(),
	target 		  :: generic_client_identifier(),
	caller 		  :: generic_dialog_party_identifier(),
	callee 		  :: generic_dialog_party_identifier(),
	upstreamRoute  	  :: route_path(),
	downstreamRoute   :: route_path(),
	routeToRecord 	  :: route_path(),
	sequenceNum	  :: integer() ,
	specificProtocol  :: any(),
	timeToLive 		:: non_neg_integer(),
	receivedOn		:: term()
}).
