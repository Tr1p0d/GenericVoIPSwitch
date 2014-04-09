-module(generic_exchange_gateway_sup).

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

SIPGatewaySpecs = 
	{
		sip_gateway,
		{
			generic_exchange_sip_gateway,
			start_link,
			[]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_sip_gateway]
	},

LCPGatewaySpecs = 
	{
		lcp_gateway,
		{
			generic_exchange_lcp_gateway,
			start_link,
			[ets:new(lCPClientTable, [bag, public, named_table])]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_lcp_gateway]
	},

LCPClientSupSpecs = 
	{
		lcp_client_sup,
		{
			generic_exchange_lcp_client_sup,
			start_link,
			[]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_lcp_client_sup]
	},

    {ok, { {one_for_one, 5, 10}, [
				SIPGatewaySpecs,
				LCPClientSupSpecs,
				LCPGatewaySpecs
	]}}.

