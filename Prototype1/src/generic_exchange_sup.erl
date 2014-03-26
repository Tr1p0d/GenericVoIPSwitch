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

-spec start_link() ->
	{ok, pid()} | {error, term()}.

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
			[]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_transport_sup]
	},

	GatewaySupervisor =
	{
		gatewaySup,
		{
			generic_exchange_gateway_sup,
			start_link,
			[]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_transport_sup]
	},

	RouterCoreSupervisor =
	{
		routerCore,
		{
			generic_exchange_core_sup,
			start_link,
			[]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_core_sup]
	},


		
    {ok, { {one_for_one, 5, 10}, [
	RouterCoreSupervisor,
	GatewaySupervisor,
	TransportSupervisor
			]}}.

