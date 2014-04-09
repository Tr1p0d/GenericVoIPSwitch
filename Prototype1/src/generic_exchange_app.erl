-module(generic_exchange_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    generic_exchange_sup:start_link().

stop(_State) ->
    normal.
