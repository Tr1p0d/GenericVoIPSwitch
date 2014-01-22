-module(filter_example).
-export([main/0]).

main() ->
	lists:filter(fun(Num) -> Num > 2 end, [1,2,3,4,5]).



