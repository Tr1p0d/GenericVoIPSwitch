-module(basic_functionality_test).

-include_lib("eunit/include/eunit.hrl").

%boot_test() ->
%	?assertMatch({ok, _}, generic_exchange_app:start()).

teardown_test() ->
	?assertMatch(ok, generic_exchange_app:stop(state)).


