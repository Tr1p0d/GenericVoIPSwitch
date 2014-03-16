-module(basic_routing_test).

-include_lib("eunit/include/eunit.hrl").

register_test_() ->
	{setup,
		fun() -> 
				{ok, _} = generic_exchange_app:start(),
				nksip_app:start(),
				ok = nksip:start(alice, simple_sip_client, [alice],
					[
						{from, "sip:alice@127.0.0.1"},
						{transport, {udp, {127,0,0,1}, 5070}}
					])
		end,

		fun(_) ->
			ok
		end,
		
		?_assertMatch({ok, 200, _}, nksip_uac:register(alice, "sip:127.0.0.1", []))
	}.

double_register_test_() ->
	{setup,
		fun() -> 
				ok = nksip:start(bob, simple_sip_client, [bob],
					[
						{from, "sip:bob@127.0.0.1"},
						{transport, {udp, {127,0,0,1}, 5080}}
					])
		end,

		fun(_) ->
				ok
		end,
		
		?_assertMatch({ok, 200, _}, nksip_uac:register(bob, "sip:127.0.0.1", []))
	}.

invite_test_() ->
	{setup,
		fun() -> 
			ok
		end,

		fun(_) ->
				generic_exchange_app:stop(state),
				ok = nksip:stop_all()
		end,
		
		?_assertMatch({ok, 200, _}, nksip_uac:invite(bob, "sip:alice@127.0.0.1", [
					{body, nksip_sdp:new()}
				]))
	}.


