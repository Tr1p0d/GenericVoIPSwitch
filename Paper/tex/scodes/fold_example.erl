downstreamRoute = lists:foldr( 
	fun(Via, Acc) ->
		[{Via#via.domain, Via#via.port, Via#via.opts}|Acc]
	end,
	[], Vias),
