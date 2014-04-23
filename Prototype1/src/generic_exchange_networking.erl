-module(generic_exchange_networking).
-export([resolve_target/2, get_domain/0, get_port/0, get_remote_port/3,
		get_self_ip/0]).

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
	case inet:gethostname() of
		{ok, Host} ->
			Host
	end.

get_port() ->
	5060.

get_self_ip() ->
	case inet:getif() of
		{ok, List} ->
			{IP, _, _} = hd(List),
				ipv4_to_string(IP)
	end.


get_remote_port(CID, DSIP, AETS) ->
	case ets:match(AETS, {CID, {DSIP, '$1'}, '_'}) of
		[[Val]] ->
			Val;
		[] ->
			{error, not_found}
	end.

ipv4_to_string({Oct1, Oct2, Oct3, Oct4}) ->
	PreIP = lists:foldr( 
		fun(Elem, Acc) ->
			[ "." | [ integer_to_list(Elem) | Acc]]
		end, 
		[], [ Oct2| [ Oct3 | [Oct4 | []]]]),
	[ integer_to_list(Oct1) | PreIP] .
