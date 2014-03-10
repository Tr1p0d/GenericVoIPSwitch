-module(bootshutdown_test).

-include_lib("eunit/include/eunit.hrl").


boot_test() ->
	[?_assertMatch({ok,_PID}, generic_exchange_app:start())].

