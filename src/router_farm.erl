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
-export([start/10,help/0]).

help() ->
    io:format("start(WorkerId, Prefix, ClientNum, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout).",[]).

start(WorkerId, Prefix, ClientNum, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout) ->
    error_logger:logfile({open, Prefix ++ "_routers.log"}),
    error_logger:tty(false),
    T = ets:new(list_to_atom("routers_" ++ Prefix),[named_table]),
    Logger = lib_misc:get_logger("/tmp/" ++ integer_to_list(WorkerId) ++ "_stat.log"),
    spawn_link(fun() -> 
		       spit_mon_log(Logger, 1000, ClientNum, T)
	       end),
    start_loop(Prefix, 0, ClientNum, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, T),
    error_logger:info_msg("All clients started : ~p~n", [ClientNum]),
    monitor_client(T, ClientNum, 0).

spit_mon_log(Logger, Sleep, ClientNum, Table) ->
    Count = ets:info(Table,size),
    Logger("total=~p,alive=~p~n",[ClientNum, Count]),
    lib_misc:sleep(Sleep),
    spit_mon_log(Logger, Sleep, ClientNum, Table).


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
		    monitor_client(T, AliveCount - 1, DiedCount + 1)
	    end
    end.
    

start_loop(_, FromIndex, ClientNum, _, _, _, _, _, _, _, _) when FromIndex >= ClientNum -> ok;
start_loop(Prefix, FromIndex, ClientNum, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidTable) ->    
    error_logger:info_msg("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
    start_routers(Prefix, FromIndex, min(FromIndex + StartBatchSize, ClientNum), Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable),
    lib_misc:sleep(StartInterval),
    start_loop(Prefix, FromIndex + StartBatchSize, ClientNum, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidTable).

start_routers(_, FromIndex, FromIndex, _, _, _, _, _, _) -> ok;
start_routers(Prefix, FromIndex, ToIndex, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable) ->
  {Pid, _} = spawn_monitor(router, start, [Prefix, FromIndex, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout]),
  ets:insert(PidTable,{Pid, FromIndex}),  
  start_routers(Prefix, FromIndex + 1, ToIndex, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable).

