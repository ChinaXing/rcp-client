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
-include_lib("stdlib/include/qlc.hrl").
%% API
-export([start/10,help/0]).
-export([get_router_pid_list/1]).
-export([do_user_online/3]).
-export([get_ets_table_name/1]).

help() ->
    io:format("start(Prefix, ClientNum, UserCount, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout).",[]).

get_ets_table_name(Prefix) -> list_to_atom("routers_" ++ Prefix).

start(Prefix, ClientNum, UserCount, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout) ->
    error_logger:logfile({open, Prefix ++ "_routers.log"}),
    error_logger:tty(false),
    TableName = get_ets_table_name(Prefix),
    T = ets:new(TableName,[named_table]),
%    Logger = lib_misc:get_logger("/tmp/" ++ integer_to_list(WorkerId) ++ "_stat.log"),
%    spawn_link(fun() -> 
%		       spit_mon_log(Logger, 1000, ClientNum, T)
%	       end),
    start_loop(Prefix, 0, ClientNum, UserCount, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, T),
    error_logger:info_msg("All clients started : ~p~n", [ClientNum]),
    monitor_client(T, ClientNum, 0).

get_router_pid_list(Prefix) ->
    TableName = get_ets_table_name(Prefix),
    qlc:e(qlc:q([K || {K,_} <- ets:table(TableName)])).

do_user_online(Count, Offset, PidList) ->
    do_user_online_loop(Count, Offset, PidList, erlang:length(PidList)).

do_user_online_loop(_count, _offset, [], N) -> wait_user_online_done(N);
do_user_online_loop(Count, Offset, [Pid|Tail], N) ->
    Pid ! {self(), user_online, Offset, Count},
    do_user_online_loop(Count, Offset, Tail, N).

wait_user_online_done(0) -> ok;
wait_user_online_done(N) ->
    receive 
	{From, {error, Reason}} ->
	    error_logger:error_msg("~p~n",{From, {error, Reason}}),
	    wait_user_online_done(N - 1);
	{_, ok} ->
	    error_logger:info_msg("ok~n"),
	    wait_user_online_done(N - 1)
    end.

%% spit_mon_log(Logger, Sleep, ClientNum, Table) ->
%%     Count = ets:info(Table,size),
%%     Logger("total=~p,alive=~p~n",[ClientNum, Count]),
%%     lib_misc:sleep(Sleep),
%%     spit_mon_log(Logger, Sleep, ClientNum, Table).

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


    
    

start_loop(_, FromIndex, ClientNum, _, _, _, _, _, _, _, _, _) when FromIndex >= ClientNum -> ok;
start_loop(Prefix, FromIndex, ClientNum, UserCount, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidTable) ->    
    error_logger:info_msg("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
    start_routers(Prefix, FromIndex, min(FromIndex + StartBatchSize, ClientNum), UserCount, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable),
    lib_misc:sleep(StartInterval),
    start_loop(Prefix, FromIndex + StartBatchSize, ClientNum, UserCount, Host, Port, BindIp, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidTable).

start_routers(_, FromIndex, FromIndex, _, _,  _, _, _, _, _) -> ok;
start_routers(Prefix, FromIndex, ToIndex, UserCount, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable) ->
  {Pid, _} = spawn_monitor(router, start, [Prefix, FromIndex, UserCount, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout]),
  ets:insert(PidTable,{Pid, FromIndex}),  
  start_routers(Prefix, FromIndex + 1, ToIndex, UserCount, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout, PidTable).
