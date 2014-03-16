-module(generic_exchange_networking).
-export([resolve_target/2]).

resolve_target(Target, AA) ->
	dict:map( fun(_Inkey, Value) -> lager:notice("association  ~p : ~p" , 
	[_Inkey, Value]) end, AA ),

	case dict:find(Target, AA) of
		{ok, Value} ->
			Value;
		error ->
			{ok, {127,0,0,1}, 5070}
	end.
