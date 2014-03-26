-module(generic_exchange_core_sup).

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

DialogRouter = 
	{
		dialogRouter,
		{
			generic_exchange_dialog_router,
			start_link,
			[ets:new(dialogTable,[public, bag, named_table])]
		},
		permanent,
		2000,
		worker,
		[generic_exchange_dialog_router]
	},

ManagerSup=
	{
		dialogManagerSup,
		{
			generic_exchange_dialog_manager_sup,
			start_link,
			[]
		},
		permanent,
		2000,
		supervisor,
		[generic_exchange_dialog_manager]
	},

    {ok, { {one_for_one, 5, 10}, [
				DialogRouter,
				ManagerSup
	]}}.
