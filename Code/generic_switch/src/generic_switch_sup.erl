
-module(generic_switch_sup).

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
	GenericSwitchSIPGateway = 
	{
		sipGateway,
		{
			generic_switch_sip_gateway,
			start_link,
			[]
		},
		permanent,
		2000,
		worker,
		[client_switch_sip_gateway]
	},
	GenericClientDatabase =
	{
		genericClientDatabase,
		{
			client_database,
			start_link,
			[]
		},
		permanent,
		2000,
		worker,
		[client_database]
	},
	SIPUDPTransportSpecs = 
	{
		udpTransport,
		{
			generic_switch_sip_transport_udp,
			start_link,
			[{5060, [binary]}]
		},
		permanent,
		2000,
		worker,
		[generic_switch_sip_transport_udp]
	},

	SIPRouterSpecs = 
	{
		sipDialogRouter,
		{
			generic_switch_sip_router,
			start_link,
			[ets:new(sipClientMapTable,[ public ])]
		},
		permanent,
		2000,
		worker,
		[generic_switch_sip_router]
	},

    {ok, { {one_for_one, 5, 10}, [
				SIPUDPTransportSpecs,
				SIPRouterSpecs,
				GenericClientDatabase,
				GenericSwitchSIPGateway]
		}}.

