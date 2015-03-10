%%%-------------------------------------------------------------------
%%% @author LambdaCat
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 三月 2015 下午4:26
%%%-------------------------------------------------------------------
-module(router_farm).
-author("LambdaCat").

%% API
-export([start/8,help/0]).

help() ->
    io:format("start(Prefix, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout).",[]).

start(Prefix, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout) ->
    PidList = start_loop(Prefix, 0, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, []),
    error_logger:info_msg("All clients started : ~p~n", [PidList]),
    T = ets:new(list_to_atom("routers_" ++ Prefix),[named_table]),
    ets:insert(T, PidList),
    monitor_client(T, length(PidList), 0).


monitor_client(T, AliveCount, DiedCount) ->
    case ets:first(T) of
	'$end_of_table' ->
	    error_logger:info_msg("All clients shutdown!~n",[]);
	_ ->
	    receive
		{'DOWN', _, process, Pid, Info} ->
		    ets:delete(T,Pid),
		    error_logger:error_msg("Router exit message : ~p~n", [Info]),
		    error_logger:error_msg("Router : alive = ~p, diede = ~p ~n", [AliveCount - 1, DiedCount + 1 ]),
		    monitor_client(T, AliveCount - 1, DiedCount)
	    end
    end.
    

start_loop(_, FromIndex, ClientNum, _, _, _, _, _, _, PidStarted) when FromIndex >= ClientNum ->
	  lib_misc:list_reverse(lib_misc:list_flatten(PidStarted));
start_loop(Prefix, FromIndex, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidStarted) ->    
    error_logger:info_msg("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
    PidOfBatch = start_routers(Prefix, FromIndex, min(FromIndex + StartBatchSize, ClientNum), Host, Port, HeartbeatInterval, WaitResponseTimeout, []),
    lib_misc:sleep(StartInterval),
    start_loop(Prefix, FromIndex + StartBatchSize, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, [PidOfBatch | PidStarted]).

start_routers(_, FromIndex, FromIndex, _, _, _, _, PidStarted) -> PidStarted;
start_routers(Prefix, FromIndex, ToIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout, PidStarted) ->
  {Pid, _} = spawn_monitor(router, start, [Prefix, FromIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout]),
  start_routers(Prefix, FromIndex + 1, ToIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout, [{Pid, FromIndex} | PidStarted]).

