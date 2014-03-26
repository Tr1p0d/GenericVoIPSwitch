-module(generic_exchange_dialog_manager_sup).

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

DialogManager = 
	{
		dialogRouter,
		{
			generic_exchange_dialog_manager,
			start_link,
			[ets:new(dialogTable,[public, bag]),
				ets:new(associationTable,[public, bag])]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_dialog_manager]
	},

DialogSup = 
	{
		dialogSup,
		{
			generic_exchange_dialog_sup,
			start_link,
			[ets:new(associationTable, [public, bag])]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_dialog_sup]
	},

    {ok, { {one_for_one, 5, 10}, [
				DialogManager,
				DialogSup
	]}}.
