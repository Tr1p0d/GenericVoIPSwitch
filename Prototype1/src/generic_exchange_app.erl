-module(generic_exchange_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(any(), any()) -> ok.

start(_StartType, _StartArgs) ->
    %generic_exchange_sup:start_link().
    ok.

-spec start() -> 
	{ok, pid()} | {error, Reason::term()}.

start() ->
    generic_exchange_sup:start_link().

stop(_State) ->
    ok.
