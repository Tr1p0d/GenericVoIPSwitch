-module(generic_exchange_networking).
-export([resolve_target/2]).

resolve_target(Target, AA) ->
	%dict:map( fun(_Inkey, Value) -> lager:notice("association  ~p : ~p" , 
	%[_Inkey, Value]) end, AA ),

	case ets:match(AA, {Target, '$1'}) of
		[[Value]] ->
			{ok,Value};
		[error] ->
			{error, not_found}
	end.
