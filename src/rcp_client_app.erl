-module(rcp_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, StartArgs} = application:get_env(default_start_args),
    rcp_client_sup:start_link(StartArgs).

stop(_State) ->
    ok.
