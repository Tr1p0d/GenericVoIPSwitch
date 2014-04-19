-module(sip_to_lcp_test).

-include_lib("eunit/include/eunit.hrl").

lcp_only_test_() ->
	{foreach,
		fun() -> 
				ok = application:start(elcpcp),
				ok = application:start(generic_exchange)
		end,

		fun(_) ->
				application:stop(elcpcp),
				application:stop(generic_exchange),
				application:unload(generic_exchange),
				application:unload(elcpcp)
		end,
		[	
			%fun() ->
			%	simple_lcp_client:simple_call(),
			%	?assertMatch( [[_,_,_]] , ets:match(lCPClientTable, {'$1','$2', '$3'}))
			%end
		]
	}.


lcp_sip_simplecall_test_() ->
	{foreach,
		fun() -> 
				ok = application:start(elcpcp),
				ok = application:start(generic_exchange),
				nksip_app:start(),
				ok = nksip:start(alice, simple_sip_client, [alice],
					[
						{from, "sip:alice@127.0.0.1"},
						{transport, {udp, {127,0,0,1}, 5070}}
					]),
				{ok,200,_} =  nksip_uac:register(alice, "sip:127.0.0.1", [])
		end,

		fun(_) ->
				application:stop(elcpcp),
				application:stop(generic_exchange),
				application:unload(generic_exchange),
				application:unload(elcpcp),
				ok = nksip:stop_all()
		end,
		[
			%fun() ->
			%	simple_lcp_client:simple_call(),
			%	?assertMatch( [[_,_,_]] , ets:match(lCPClientTable, {'$1','$2', '$3'}))
			%end
		]
	}.

