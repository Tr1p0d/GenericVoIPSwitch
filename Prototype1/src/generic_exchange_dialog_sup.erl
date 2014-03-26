-module(generic_exchange_dialog_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================


start_link(AssociationETS) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) -> 
	Spec=
	{
		dialog_fsm,
		{
			generic_dialog_fsm,
			start_link,
			[]
		},
		temporary,
		2000,
		worker,
		[generic_dialog_fsm]
	},


    {ok, { {simple_one_for_one, 5, 10}, [Spec]}}.
