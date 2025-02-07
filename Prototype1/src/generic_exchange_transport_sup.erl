-module(generic_exchange_transport_sup).

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

SIPUDPTransportSpecs = 
	{
		udpTransport,
		{
			generic_exchange_transport_sip_udp,
			start_link,
			[{5060, [binary]}]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_transport_sip_udp]
	},

	generic_exchange_transport_lcp:start(),
		
    {ok, { {one_for_one, 5, 10}, [
				SIPUDPTransportSpecs
	]}}.

