-module(sip_test).

-include_lib("eunit/include/eunit.hrl").

registration_test_() ->
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
				ok = nksip:start(bob, simple_sip_client, [bob],
					[
						{from, "sip:bob@127.0.0.1"},
						{transport, {udp, {127,0,0,1}, 5080}}
					])
		end,

		fun(_) ->
				application:stop(elcpcp),
				application:stop(generic_exchange),
				ok = nksip:stop_all(),	
				application:unload(generic_exchange),
				application:unload(elcpcp)
		end,
		[	
			%fun(_) -> % registration test
			%	?_assertMatch({ok, 200, _}, nksip_uac:register(alice, "sip:127.0.0.1", []))
			%end,
			%fun(_) -> % double registration test
			%	?_assertMatch({{ok, 200, _},{ok, 200, _}},
			%			{nksip_uac:register(alice, "sip:127.0.0.1", []),
			%			nksip_uac:register(bob, "sip:127.0.0.1", [])})
			%end,
			%fun(_) ->
			%	{ok, 200, _} = nksip_uac:register(alice, "sip:127.0.0.1", []),
			%	{ok, 200, _} = nksip_uac:register(bob, "sip:127.0.0.1", []),
			%	?assertMatch({ok, 200, [{dialog_id, _}]}, 
			%		nksip_uac:invite(bob, "sip:alice@127.0.0.1",
			%		[{body, nksip_sdp:new()}]))
			%end,
			fun() -> 
				{ok, 200, _} = nksip_uac:register(alice, "sip:127.0.0.1", []),
				{ok, 200, _} = nksip_uac:register(bob, "sip:127.0.0.1", []),
				{ok, 200, [{dialog_id, Dlg}]} = nksip_uac:invite(bob, "sip:alice@127.0.0.1",
					[{body, nksip_sdp:new()}]),
				?assertMatch(ok, nksip_uac:ack(bob, Dlg, [])),
				?assertMatch({ok, 200, _}, nksip_uac:bye(bob, Dlg, []))
			end
		]
	}.

%		?assertMatch(ok, nksip_uac:ack(bob, Dlg, [])),
%		?assertMatch({ok, 200, _}, nksip_uac:bye(bob, Dlg, [])).

