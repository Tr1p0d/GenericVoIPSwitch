-type gen_resource_identificator :: nksip:uri(),

-type req_type() 	:: make_call | teardown | associate,
-type res_type() 	:: ring | accept,
-type target_uri() 	:: gen_resource_identificator(),
-type route() 		:: [ gen_resource_identificator() ],
-type headers()		:: [string()]

-record(gen_msg, {
	type 			:: {req, req_type()} | {res, res_type()},
	target 			:: target_uri(),
	caller 			:: 
	callee 			::
	upstreamRoute  	:: route(),
	downstreamRoute :: route(),
	routeToRecord 	:: route(),
	sequenceNum		:: integer(),
	specificProtocol :: string()
}).

