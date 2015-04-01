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
%-export([start/7, start_router/5]).
-compile(export_all).
-include("package_constant.hrl").

start(Prefix, Index, Host, Port, BindIp, HeartbeatInterval, WaitTimeout) ->
    Logger = lib_misc:gen_logger("Router : ~p, ", [Index]),
    {ok, IOPid} = start_router(Host, Port, BindIp, WaitTimeout, Logger),
    %% start auth
    case router_auth:do_auth(IOPid, {Prefix, Index, WaitTimeout, 5, Logger}) of
	{error, Reason} ->
	    exit(Reason);
	ok ->
	    ok
    end,
    %% start heartbeat
    {ok, _} = router_heartbeat:heart_beat_loop(
		       IOPid, {Prefix, Index, WaitTimeout, HeartbeatInterval, Logger}),
    %% listen 
    command_listen(Logger).

command_listen(Logger) ->
    receive
	{_, error, Reason} ->
	    Logger(error,"heartbeat failed, exit : ~p~n",[Reason]),
	    exit(Reason)
    end.
    

%% ------------------------------------------------------------------------------------------------
%% 启动一个Io进程，接收业务进程发送的IO请求
%% 1. send -> 发送数据包，
%% 2. recieve -> 接收到数据包的时候解多路复用到对应的业务进程
%% ------------------------------------------------------------------------------------------------
start_router(Host, Port, BindIp, WaitTimeout, Logger) ->
    Logger(info, "begin connect to rcp ~n", []),
    {ok, Socket} = gen_tcp:connect(Host, Port, 
				   [{ip, BindIp}, {packet, 2}, 
				    binary,{active, true}, 
				    {send_timeout, WaitTimeout}], WaitTimeout),
    Logger(info, "connected~n", []),
    Pid = spawn_link(?MODULE, io_loop, [Socket, Logger]),
    ok = gen_tcp:controlling_process(Socket,Pid),
    {ok, Pid}. 

%% IO 进程的主循环
io_loop(Socket, Logger) ->
    io:format("hello, io_loop~n",[]),
    receive 
	{tcp, Socket, Data} ->
	    case demultiplex(Data) of %% 解多路复用，分解到具体的业务进程处理
		{ok, Target} ->
		    Target ! {response, Data},
		    io_loop(Socket, Logger);
		{error, Reason} ->
		    Logger(error, "cannot demultiplex response Data to biz Packet,~p~n",[Reason])
	    end;
	{From, Type, Packet} ->
	    put("packet_type_" ++ Type, From), %% save packet type owner / for later demultiplex
	    From ! gen_tcp:send(Socket, Packet),
	    io_loop(Socket, Logger);
	{From, exit} ->
	    From ! ok;
	Other ->
	    Logger("unknow command ~p~n",[Other])
    end.


%% demultiplex
demultiplex(Data) ->
    Type = packet_farm:get_packet_command(Data),
    case Type of
	{unknow, Reason} ->
	    {error, Reason};
	Type ->
	    Target = get("packet_type_" ++ Type),
	    {ok, Target}
    end.
    
	
    
