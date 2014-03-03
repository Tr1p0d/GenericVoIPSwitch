-module(generic_switch_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	start(bla,bla).

start(_StartType, _StartArgs) ->
	lager:start(),
    generic_switch_sup:start_link().


stop(_State) ->
    ok.
