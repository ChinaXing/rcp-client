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
-export([start/7, start_router/4]).
-include("package_constant.hrl").

start(Prefix, Index, UserCount, Host, Port, HeartbeatInterval, WaitTimeout) ->
  Logger = lib_misc:get_logger("Router[~p]", [Index]),
  {ok, IOPid} = start_router(Host, Port, WaitTimeout, Logger),
  %% start auth
  case router_auth:do_auth(IOPid, {Prefix, Index, WaitTimeout, 5}, Logger) of
    {error, Reason} ->
      exit(Reason);
    ok ->
      ok
  end,
  %% start heartbeat
  {ok, _} = router_heartbeat:heart_beat_loop(
    IOPid, {Prefix, Index, WaitTimeout, HeartbeatInterval}, Logger),
  % sleep a while
  lib_misc:sleep(5000),
  [C | _] = Prefix,
  I = list_to_integer([C]),
  % start user online
  RouterIndex = I bsl 16 + Index,
  UserOnlineLogFun = fun(Type, Strformat, Args) -> Logger({[{context, user_online}], Type}, Strformat, Args) end,  
  router_user_online:do_online(IOPid, {UserCount, Prefix, RouterIndex, WaitTimeout}, UserOnlineLogFun),

  %% listen
  command_listen(Prefix, IOPid, RouterIndex, WaitTimeout, Logger).

command_listen(Prefix, IOPid, RouterIndex, WaitTimeout, Logger) ->
  receive
    {From, user_online, Offset, Count} ->
      Ret = router_user_online:do_online(IOPid,
        {Count, Prefix, RouterIndex + Offset, WaitTimeout}, lib_misc:get_logger(Logger, user_online)
      ),
      From ! {self(), Ret},
      command_listen(Prefix, IOPid, RouterIndex, WaitTimeout, Logger);
    {_, error, Reason} ->
      Logger(error, "router exit , reason : ~p~n", [Reason]),
      exit(Reason)
  end.


%% ------------------------------------------------------------------------------------------------
%% 启动一个Io进程，接收业务进程发送的IO请求
%% 1. send -> 发送数据包，
%% 2. recieve -> 接收到数据包的时候解多路复用到对应的业务进程
%% ------------------------------------------------------------------------------------------------
start_router(Host, Port, WaitTimeout, Logger) ->
  Logger(info, "begin connect to rcp ~n", []),
  Start = lib_misc:get_timestamp_micro_seconds(),
  {ok, Socket} = gen_tcp:connect(Host, Port,
    [{packet, 2},
      binary, {active, true},
      {send_timeout, WaitTimeout}], WaitTimeout),
  End = lib_misc:get_timestamp_micro_seconds(),
  Logger(info, "connected, connect_rt=~p~n", [End - Start]),
  Pid = spawn_link(fun() ->
    io_loop(Socket, Logger)
  end),
  ok = gen_tcp:controlling_process(Socket, Pid),
  {ok, Pid}.

%% IO 进程的主循环
io_loop(Socket, Logger) ->
  receive
    {tcp, Socket, Data} ->
      case demultiplex(Data) of %% 解多路复用，分解到具体的业务进程处理
        {ok, Target} ->
          Target ! {response, Data},
          io_loop(Socket, Logger);
        {error, Reason} ->
          Logger(warn, "[ignore] cannot demultiplex response Data to biz Packet,~p~n", [Reason]),
          io_loop(Socket, Logger)
      end;
    {tcp_closed, Socket} ->
      Logger(error, "socket closed !~n", []),
      exit(tcp_closed);
    {tcp_error, Socket, Reason} ->
      Logger(error, "socket error : ~p~n", [Reason]),
      exit(Reason);
    {From, Type, Packet} ->
      put("packet_type_" ++ Type, From), %% save packet type owner / for later demultiplex
      From ! gen_tcp:send(Socket, Packet),
      io_loop(Socket, Logger);
    {From, exit} ->
      From ! ok,
      exit(processor_exit);
    Other ->
      Logger(error, "unknow command ~p~n", [Other]),
      exit(unknow_command)
  end.


%% demultiplex
demultiplex(Data) ->
  Type = packet_farm:get_packet_command(Data),
  case Type of
    {unknow, Reason} ->
      {error, Reason};
    Type ->
      case packet_farm:get_packet_command_req(Type) of
        {unknow, Reason} ->
          {error, {unknow, Reason}};
        {ok, ReqCmds} ->
          find_req_by_cmds(ReqCmds)
      end
  end.
find_req_by_cmds([Cmd | Cmds]) ->
  case get("packet_type_" ++ Cmd) of
    undefined ->
      find_req_by_cmds(Cmds);
    Target ->
      {ok, Target}
  end;
find_req_by_cmds([]) -> {error, {no_registered_cmd_processor}}.
