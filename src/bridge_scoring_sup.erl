-module(bridge_scoring_sup).

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
	MaxRestart = 5,
	MaxTime = 10,
	ShutdownTime = 6000,
    {ok, { {one_for_one, MaxRestart, MaxTime}, 
	 [{bs_server,
		{bs_server, start_link, []},
		transient,
		ShutdownTime,
		worker,
		[bs_server]
	  }]} }.

