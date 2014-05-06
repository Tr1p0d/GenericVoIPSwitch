{
	make_call, % message type identifier
	<<1016>>,  % target
	{
		<<1016>>,
		unique_call_id,
		unique_caller_part_id
	}, % unique caller dialog identifier
	{
		<<1017>>,
		unique_call_id,
		unique_callee_part_id
	}, % unique callee dialog identifier
	[], % upstream route
	[], % route to record
	[{
		{127,0,0,1}, 5070
	}], % downstream route
	3154, % sequence number
	70, % time to live
	{{127,0,0,1}, 5060} % packet arrival interface
}
