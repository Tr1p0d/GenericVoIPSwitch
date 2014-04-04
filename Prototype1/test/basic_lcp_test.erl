-module(basic_lcp_test).

-include_lib("eunit/include/eunit.hrl").

simple_message_test_() ->
	{setup,
		fun() -> 
				ok
		end,

		fun(_) ->
			ok
		end,
		
		?_assertMatch(ok, simple_lcp_client:send_message())
	}.





