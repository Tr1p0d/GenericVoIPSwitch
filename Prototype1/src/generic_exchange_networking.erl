-module(generic_exchange_networking).
-export([resolve_target/2, get_domain/0, get_port/0]).

resolve_target(Target, AA) ->

	lager:info('looking for ~p in association table state ~p', 
		[Target, ets:match(AA, {'$1', '$2', '$3'})]),

	case ets:match(AA, {Target, '$1', '_'}) of
		[[Value]] ->
			{ok,Value};
		[] ->
			{error, not_found}
	end.

get_domain() ->
	<<"127.0.0.1">>.

get_port() ->
	5060.
