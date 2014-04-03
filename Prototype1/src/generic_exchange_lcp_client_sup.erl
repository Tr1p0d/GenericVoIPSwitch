-module(generic_exchange_lcp_client_sup).

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
	Spec=
	{
		lcp_client,
		{
			generic_exchange_lcp_client_fsm,
			start_link,
			[]
		},
		temporary,
		2000,
		worker,
		[generic_exchange_lcp_client_fsm]
	},


    {ok, { {simple_one_for_one, 5, 10}, [Spec]}}.
