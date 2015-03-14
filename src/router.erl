%%%-------------------------------------------------------------------
%%% @author LambdaCat
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 三月 2015 下午4:28
%%%-------------------------------------------------------------------
-module(router).
-author("LambdaCat").

%% API
-export([start/7]).
-include("package_constant.hrl").

start(Prefix, Index, Host, Port, BindIp, HeartbeatInterval, WaitResponseTimeout) ->
    lib_misc:lib_init(Index),
    Logger = lib_misc:gen_logger("Router : ~p, ", [Index]),
    Logger(info, "begin connect to rcp ~n", []),
    {ok, Socket} = gen_tcp:connect(Host, Port, [{ip, BindIp}, {packet, 0}, binary,{active, false}]),
    Logger(info, "connected~n", []),
    Logger(info, "start Auth~n", []),
    case do_auth(Prefix, Index, Socket, WaitResponseTimeout, 5, Logger) of
	{error, Reason} ->
	    exit(Reason);
	ok ->
	    Logger(info, "auth ok~n", []),
	    Logger(info, "start heartbeat~n", []),
	    case do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, 10, 10, Logger) of
		{error, Reason} ->
		    exit(Reason);
		ok ->
		    ok
	    end
    end.


%% 认证
do_auth(Prefix, Index, Socket, WaitResponseTimeout, RetryTime, Logger) ->
    Package = packet_farm:build_package(auth, Prefix, Index,[]),
    case gen_tcp:send(Socket, Package) of
	{error, Reason} ->
	    Logger(error, "send auth package failed : ~p ~n", [Reason]),
	    if
		RetryTime == 0 ->
		    Logger(error, "send auth failed after retries~n", []),
		    {error, Reason};
		true ->
		    Logger(error, "retry send auth : ~p~n", [RetryTime]),
		    do_auth(Prefix, Index, Socket, WaitResponseTimeout, RetryTime - 1, Logger)
	    end;
	ok ->
	    case gen_tcp:recv(Socket, ?AUTH_RESPONSE_LENGTH, WaitResponseTimeout) of
		{error, Reason} -> 
		    Logger(error, "receive auth failed : ~p~n", [ Reason ]),
		    {error, Reason};
		{ok, Packet} ->
		    check_auth_response(Packet)
	    end
    end.

%% 心跳，失败则连续尝试 MaxRetryTime 后，返回失败
%%
%% 频率 : HeartbeatInterval +/- round(HeartbeatInterval/3) 
%%
%% 在每次发起心跳的之前，都进行暂停，无论是失败尝试，还是正常的心跳
%%
do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime, Logger) ->
    Package = packet_farm:build_package(heartbeat, Prefix, true, [200]),
    lib_misc:sleep_random(HeartbeatInterval, round(HeartbeatInterval / 3)),
    case gen_tcp:send(Socket, Package) of
	{error, Reason} ->
	    Logger(error, "send heartbeat package failed : ~p ~n", [Reason]),
	    if
		RetryTime == 0 ->
		    Logger(error, "send heartbeat failed after retries~n", []),
		    {error, Reason};
		true ->
		    Logger(error, "retry send heartbeat : ~p~n", [RetryTime]),
		    do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime - 1, Logger)
	    end;
	ok ->
	    Logger(info, "send heartbeat request ok~n",[]),
	    case gen_tcp:recv(Socket, ?HEART_BEAT_RESPONSE_LENGTH, WaitResponseTimeout) of
		{error, Reason} -> 
		    Logger(error, "receive heartbeat error : ~p~n",[Reason]),
		    if
			RetryTime == 0 ->
			    Logger(error, "heartbeat failed after reties ~n", []),
			    {error, Reason};
			true ->
			    do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime - 1, Logger)
		    end;
		{ok, Packet} ->
		    case check_heart_beat_response(Packet) of
			ok ->
			    Logger(info, "receive heartbeat is valid~n",[]),
			    do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, MaxRetryTime, Logger);
			{error, Reason} ->
			    Logger(info, "check heartbeat response failed :~p ~n",[Reason]),
			    if
				RetryTime == 0 ->
				    Logger(error, "heartbeat failed after reties ~n", []),
				    {error, Reason};
				true ->
				    do_heart_beat(Prefix, Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime - 1, Logger)
			    end
		    end
	    end
    end.


check_auth_response(Package) ->
    {ok, {Success, Code}} = packet_farm:parse_package(auth, Package),
    if 
	Success == 1 -> ok;
	true -> {error, "code is " ++ integer_to_list(Code) }
    end.

check_heart_beat_response(Package) ->
    ok = packet_farm:parse_package(heartbeat, Package),
    ok.


