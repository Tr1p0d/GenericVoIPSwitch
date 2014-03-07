-module(generic_exchange_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    %generic_exchange_sup:start_link().
    ok.

start() ->
    generic_exchange_sup:start_link().

stop(_State) ->
    ok.
