-module(bootshutdown_test.erl).
-include("eunit/include/eunit.hrl").


boot_test() ->
	[?_assertMatch({ok,_PID}, generic_exchange_app:start())].

