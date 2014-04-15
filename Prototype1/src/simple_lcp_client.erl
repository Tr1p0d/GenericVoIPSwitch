-module(simple_lcp_client).
-compile(export_all).


-include("../deps/elcpcp/src/msg_type.hrl").

simple_call() ->
    {ok, ClientSocket1} = gen_udp:open(10001, [binary, {active, false}]),
    gen_udp:send(ClientSocket1, {127,0,0,1}, 4066, <<2#11000000, "cookie">>),
	timer:sleep(1000),
	gen_udp:send(ClientSocket1, {127,0,0,1}, 4066, 
		lcp_msg:create(#datagram_ind{protocol=cornet_msg,
				msg={kbd_down_ind, 0}})),
	timer:sleep(2000).
	



		

