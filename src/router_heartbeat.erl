-module(router_heartbeat).
-export([heart_beat_loop/2,do_heart_beat/2]).

%% 不停地执行心跳，固定的频率，一旦失败，就退出
-spec heart_beat_loop(pid(),
		    { Prefix :: string(),
		      Index :: integer(),
		      WaitTimeout :: integer(),
		      HeartbeatInterval :: integer(),
		      Logger :: fun()}
		   ) -> {ok, RepeaterPid :: pid()}.
%% ---------------------------------------------------------
%% 执行定时的心跳
%% 1. 返回一个心跳进程的Pid，如果心跳出错，此进程退出，并发送给调用者一个失败的消息
%% 2. 调用者可以控制此心跳进程的退出
%% ---------------------------------------------------------
heart_beat_loop(IOPid, {Prefix, Index, WaitTimeout, HeartbeatInterval, Logger}) ->
    HeartbeatExecutor = spawn(heart_beat_executor,[IOPid, {Prefix, Index, WaitTimeout, Logger}]),
    Caller = self(),
    Repeater = spawn(fun() ->
			     Ret = heart_beat_fix_rate(HeartbeatExecutor, HeartbeatInterval),
			     Caller ! {self(), Ret}
		     end),
    {ok, Repeater}.

%% ----------------------------------------------------------
%% 按照固定的频率来启动另一个执行器
%% 1. 通过发送命令然后等待超时
%% 2. 如果中途收到错误，那么直接返回
%% 3. 如果收到退出的通知，则停止executor，然后自己退出 
%% ----------------------------------------------------------
heart_beat_fix_rate(HeartbeatExecutor, HeartbeatInterval) ->
    HeartbeatExecutor ! {self(), start},
    receive 
	{_, error, Reason} ->
	    {error, Reason};
	{From, exit} ->
	    HeartbeatExecutor ! {self(), exit},
	    From ! ok
    after HeartbeatInterval ->
	    heart_beat_fix_rate(HeartbeatExecutor, HeartbeatInterval)
    end.

%% -----------------------------------------------------------
%% 等待其它进程发来的启动消息，然后执行心跳
%% 1. 如果出错则发送错误消息给启动者，然后退出
%% 2. 否则一直等待新的启动消息
%% -----------------------------------------------------------
heart_beat_executor(IOPid, {Prefix, Index, WaitTimeout, Logger}) ->
    receive 
	{From, start} ->
	    case do_heart_beat(IOPid, {Prefix, Index, WaitTimeout, 5, Logger}) of
		ok ->
		    heart_beat_executor(IOPid, {Prefix, Index, WaitTimeout, Logger});
		{error, Reason} ->
		    From ! {self(), error, Reason}
	    end;
	{From, exit} ->
	    From ! {self(), ok}
    end.
		

%% 执行一次心跳
-spec do_heart_beat(pid(),
		    { Prefix :: string(),
		      Index :: integer(),
		      WaitTimeout :: integer(),
		      HeartbeatInterval :: integer(),
		      Retry :: integer(),
		      Logger :: fun()}
		   ) -> ok | {error, Reason :: any()}.

do_heart_beat(IOPid, {Prefix, Index, WaitTimeout, Retry, Logger}) ->
    Packet = packet_farm:build_package(heartbeat, Prefix, true, [200]),
    Start = lib_misc:get_timestamp_micro_seconds(),
    IOPid ! {self(), heartbeat, Packet},
    receive
	{error, Reason} ->
	    Logger(error, "send heartbeat package failed : ~p ~n", [Reason]),
	    if
		Retry == 1 ->
		    Logger(error, "send heartbeat failed after retries~n", []),
		    {error, Reason};
		true ->
		    Logger(error, "retry send heartbeat : ~p~n", [Retry]),
		    do_heart_beat(IOPid, {Prefix, Index, WaitTimeout, Retry - 1, Logger})
	    end;
	ok ->
	    Logger(info, "send heartbeat request ok~n",[]),
	    receive
		{error, Reason} -> 
		    Logger(error, "receive heartbeat error : ~p~n",[Reason]),
		    if
			Retry == 1 ->
			    Logger(error, "heartbeat failed after reties ~n", []),
			    {error, Reason};
			true ->
			    do_heart_beat(IOPid, {Prefix, Index, WaitTimeout, Retry - 1, Logger})
		    end;
		{response, Response} ->
		    case check_heart_beat(Response) of
			ok ->
			    End = lib_misc:get_timestamp_micro_seconds(),
			    Logger(info, "receive heartbeat is valid, rt=~p~n",[End - Start]);
			{error, Reason} ->
			    Logger(info, "check heartbeat response failed :~p ~n",[Reason]),
			    if
				Retry == 1 ->
				    Logger(error, "heartbeat failed after reties ~n", []),
				    {error, Reason};
				true ->
				    do_heart_beat(IOPid, {Prefix, Index, WaitTimeout, Retry - 1, Logger})
			    end
		    end
	    after WaitTimeout ->
		    Logger(error, "heartbeat recieve response timeout~n",[]),
		    {error, timeout}
	    end
    end.

%% check heartbeat    
check_heart_beat(Response) ->
    packet_farm:parse_package(heartbeat, Response).

