-module(generic_exchange_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	TransportSupervisor =
	{
		transportSup,
		{
			generic_exchange_transport_sup,
			start_link,
			[ets:new(dialogETS, {set, protected})]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_transport_sup]
	},

    {ok, { {one_for_one, 5, 10}, [
	TransportSupervisor
			]}}.

