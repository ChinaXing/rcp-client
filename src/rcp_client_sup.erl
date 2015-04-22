-module(rcp_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, RestartStrategy, Args), {I, {I, start_link, Args}, RestartStrategy, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    {ok, { 
       {one_for_one, 
	0, % max restarts
	10 % max seconds between restart
       }, 
       [
	?CHILD(router_farm,worker, temporary, Args)
       ]
      }
    }.

