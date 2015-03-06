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
-export([start/7,hello_erlang/1]).

hello_erlang(A) -> 
    io:format("~p~n",[A]).

start(ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout) ->
    PidList = start_loop(0, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, []),
    io:format("All clients started : ~p~n", [PidList]),
    T = ets:new(routers,[]),
    ets:insert(T, PidList),
    monitor_client(T).


monitor_client(T) ->
    case ets:first(T) of
	'$end_of_table' ->
	    io:format("All clients shutdown!~n",[]);
	_ ->
	    receive
		{'DOWN', _, process, Pid, Info} ->
		    ets:delete(T,Pid),
		    io:format("Router exit message : ~p~n", [Info]),
		    monitor_client(T)
	    end
    end.
    

start_loop(FromIndex, ClientNum, _, _, _, _, _, _, PidStarted) when FromIndex >= ClientNum ->
	  lib_misc:list_reverse(lib_misc:list_flatten(PidStarted));
start_loop(FromIndex, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, PidStarted) ->    
    io:format("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
    PidOfBatch = start_routers(FromIndex, min(FromIndex + StartBatchSize, ClientNum), Host, Port, HeartbeatInterval, WaitResponseTimeout, []),
    lib_misc:sleep(StartInterval),
    start_loop(FromIndex + StartBatchSize, ClientNum, Host, Port, StartInterval, StartBatchSize, HeartbeatInterval, WaitResponseTimeout, [PidOfBatch | PidStarted]).

start_routers(FromIndex, FromIndex, _, _, _, _, PidStarted) -> PidStarted;
start_routers(FromIndex, ToIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout, PidStarted) ->
  {Pid, _} = spawn_monitor(router, start, [FromIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout]),
  start_routers(FromIndex + 1, ToIndex, Host, Port, HeartbeatInterval, WaitResponseTimeout, [{FromIndex, Pid} | PidStarted]).

