-module(simple_lcp_client).
-compile(export_all).

send_message() ->
    {ok, ClientSocket1} = gen_udp:open(10001, [binary, {active, false}]),
    gen_udp:send(ClientSocket1, {127,0,0,1}, 4066, <<2#11000000, "cookie">>).

		

