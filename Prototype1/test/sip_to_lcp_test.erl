-module(sip_to_lcp_test).

-include_lib("eunit/include/eunit.hrl").

simple_message_test_() ->
	{setup,
		fun() -> 
			generic_exchange_app:start(),
				nksip_app:start(),
				ok = nksip:start(alice, simple_sip_client, [alice],
					[
						{from, "sip:alice@127.0.0.1"},
						{transport, {udp, {127,0,0,1}, 5070}}
					]),
			nksip_uac:register(alice, "sip:127.0.0.1", [])
		end,

		fun(_) ->
			ok
		end,
		
		?_assertMatch(ok, simple_lcp_client:send_message())
	}.
