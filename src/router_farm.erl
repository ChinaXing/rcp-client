%%%-------------------------------------------------------------------
%%% @author LambdaCat
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 三月 2015 下午4:26
%%%-------------------------------------------------------------------
-module(router_farm).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-author("LambdaCat").
-include_lib("stdlib/include/qlc.hrl").
-record(state, {start_args}).
%% API
-export([
	 start_link/1,
	 stop/0,
	 start_routers/0,
	 set_start_param/1,
	 get_router_pid_list/0,
	 user_online/3,
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	 ]).

terminate(Reason, State) ->
    lager:error("Stoped ~p, s : ~p ~n",[Reason, State]),
    ok.

handle_info(Info, State) ->
    lager:info("~p~n", [Info]),
    {noreply, State}.

code_change(_A, S, _C) ->
    {ok, S}.

start_link(Args) ->
    gen_server:start_link(
      {local, ?SERVER},
      ?MODULE,
      Args,
      []).

stop() ->
    gen_server:cast(
      ?SERVER,
      stop).
start_routers() ->
    gen_server:cast(
      ?SERVER,
      start_routers
     ).
get_router_pid_list() ->
    gen_server:call(
      ?SERVER,
      get_router_pid_list).
user_online(Count,Offset, PidList) ->
    gen_server:call(
      ?SERVER,
      {do_user_online, {Count,Offset,PidList}}
      ).
set_start_param(Options) ->
    gen_server:call(
      ?SERVER,
      {set_start_param, Options}
     ).
init(Args) ->
    {ok, #state{ start_args = Args }}.

handle_cast(start_routers, #state{ start_args = StartArgs} = State) ->
    lager:info("start routers ...,~n",[]),
    Prefix = maps:get(prefix, StartArgs),
    TableName = case maps:find(table_name,StartArgs) of
		    {ok, T } ->
			T;
		    error ->
			T = list_to_atom("routers_" ++ Prefix),
			maps:put(table_name, T, StartArgs),
			ets:new(T,[named_table]),
			T
		end,
    ClientNum = maps:get(client_num, StartArgs),
    do_start_routers(Prefix, 0, ClientNum, StartArgs),
    lager:info("All clients started : ~p~n", [ClientNum]),
    spawn_link(fun() ->
		       monitor_client(TableName, ClientNum, 0)
	       end),
    {reply, ok, State}.

% 设置参数
handle_call({set_start_param, Options}, _From, #state{ start_args = StartArgs } = State) ->
    NM = maps:from_list(Options),
    NArgs = maps:merge(StartArgs,NM),
    {reply,
     NArgs,
     State#state{ start_args = NArgs }
    };

handle_call(get_router_pid_list, _From, #state{ start_args = StartArgs } = State) ->
    TableName = maps:get(table_name, StartArgs),
    Rt = qlc:e(qlc:q([K || {K,_} <- ets:table(TableName)])),
    {reply, Rt, State};

handle_call({do_user_online,{Count, Offset, PidList}},
	     _From, State ) ->
		   do_user_online_loop(Count, Offset, PidList, erlang:length(PidList)),
		   {reply, ok, State}.
		   
do_user_online_loop(_count, _offset, [], N) -> wait_user_online_done(N);
do_user_online_loop(Count, Offset, [Pid|Tail], N) ->
    Pid ! {self(), user_online, Offset, Count},
    do_user_online_loop(Count, Offset, Tail, N).

wait_user_online_done(0) -> 
    error_logger:info_msg("wait user_online Finished~n",[]),
    ok;
wait_user_online_done(N) ->
    error_logger:info_msg("wait user_online done ...~n",[]),
    receive 
	{From, {error, Reason}} ->
	    lager:error("~p~n",[{From, {error, Reason}}]),
	    wait_user_online_done(N - 1);
	{_, ok} ->
	    lager:info("ok~n",[]),
	    wait_user_online_done(N - 1)
    end.

monitor_client(T, AliveCount, DiedCount) ->
    case ets:first(T) of
	'$end_of_table' ->
	    lager:info("All clients shutdown!~n",[]);
	_ ->
	    receive
		{'DOWN', _, process, Pid, Info} ->
		    ets:delete(T,Pid),
		    lager:error("Router exit message : ~p~n", [Info]),
		    lager:error("Router : alive = ~p, diede = ~p ~n", [AliveCount - 1, DiedCount + 1 ]),
		    monitor_client(T, AliveCount - 1, DiedCount + 1)
	    end
    end.
    

do_start_routers(_Prefix, FromIndex, ClientNum, _Map) when FromIndex >= ClientNum -> ok;
do_start_routers(Prefix, FromIndex, ClientNum, Map) -> 
    StartBatchSize = maps:get(batch_size,Map),
    lager:info("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
    do_parallel_start_routers(Prefix, FromIndex, min(FromIndex + StartBatchSize, ClientNum), Map),
    lib_misc:sleep(1000),
    do_start_routers(Prefix, FromIndex + StartBatchSize, ClientNum, Map).

do_parallel_start_routers(_, FromIndex, FromIndex, _Map) -> ok;
do_parallel_start_routers(Prefix, FromIndex, ToIndex, Map) ->
    Prefix = maps:get(prefix, Map),
    UserCount = maps:get(user_count, Map),
    Host = maps:get(rcp_host, Map),
    Port = maps:get(rcp_port, Map),
    Timeout = maps:get(timeout, Map),
    HeartBeatInterval = maps:get(heartbeat_interval, Map),
    {Pid, _} = spawn_monitor(router, start, [Prefix, FromIndex, UserCount, Host, Port, HeartBeatInterval, Timeout]),
    PidTable = maps:get(table_name, Map),
    ets:insert(PidTable,{Pid, FromIndex}),  
    do_parallel_start_routers(Prefix, FromIndex + 1, ToIndex, Map).
