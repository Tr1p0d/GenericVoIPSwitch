-module(generic_exchange_networking).
-export([resolve_target/2, get_domain/0, get_port/0, get_remote_port/3]).

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

get_remote_port(CID, DSIP, AETS) ->
	case ets:match(AETS, {CID, {DSIP, '$1'}, '_'}) of
		[[Val]] ->
			Val;
		[] ->
			{error, not_found}
	end.
