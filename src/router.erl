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
-export([start/5]).
-include("package_constant.hrl").

start(Index, Host, Port, HeartbeatInterval, WaitResponseTimeout) ->
  Logger = lib_misc:gen_logger("Router : ~p, ", [Index]),
  Logger("begin connect to rcp ~n", []),
  {ok, Socket} = gen_tcp:connect(Host, Port, [{packet, 0}, binary,{active, false}]),
  Logger("connected~n", []),
  Logger("start Auth~n", []),
  case do_auth(Index, Socket, WaitResponseTimeout, 5, Logger) of
    {error, Reason} ->
      exit(Reason);
    ok ->
      Logger("auth ok~n", []),
      Logger("start heartbeat~n", []),
      case do_heart_beat(Index, Socket, HeartbeatInterval, WaitResponseTimeout, 10, 10, Logger) of
        {error, Reason} ->
          exit(Reason);
        ok ->
          ok
      end
  end.



do_auth(Index, Socket, WaitResponseTimeout, RetryTime, Logger) ->
  Package = packet_farm:build_package(auth, Index,[]),
  case gen_tcp:send(Socket, Package) of
    {error, Reason} ->
      Logger("send auth package failed : ~p ~n", [Reason]),
      if
        RetryTime == 0 ->
          Logger("send auth failed after retries~n", []),
          {error, Reason};
        true ->
          Logger("retry send auth : ~p~n", [RetryTime]),
          do_auth(Index, Socket, WaitResponseTimeout, RetryTime - 1, Logger)
      end;
    ok ->
      case gen_tcp:recv(Socket, ?AUTH_RESPONSE_LENGTH, WaitResponseTimeout) of
        {error, Reason} -> {error, Reason};
        {ok, Packet} ->
          check_auth_response(Packet)
      end
  end.

do_heart_beat(Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime, Logger) ->
  Package = packet_farm:build_package(heartbeat, true, [200]),
  case gen_tcp:send(Socket, Package) of
    {error, Reason} ->
	  Logger("send heartbeat package failed : ~p ~n", [Reason]),
	  if
	      RetryTime == 0 ->
		  Logger("send heartbeat failed after retries~n", []),
		  {error, Reason};
	      true ->
		  Logger("retry send heartbeat : ~p~n", [RetryTime]),
		  do_heart_beat(Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime - 1, Logger)
	  end;
    ok ->
	  Logger("send heartbeat request ok~n",[]),
	  case gen_tcp:recv(Socket, ?HEART_BEAT_RESPONSE_LENGTH, WaitResponseTimeout) of
	  {error, Reason} -> {error, Reason};
	  {ok, Packet} ->
		  case check_heart_beat_response(Packet) of
		      ok ->
			  Logger("receive heartbeat is valid~n",[]),
			  lib_misc:sleep(HeartbeatInterval),
			  do_heart_beat(Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, MaxRetryTime, Logger);
		      {error, Reason} ->
			  Logger("receive heartbeat response ok~n",[]),
			  if
			      RetryTime == 0 ->
				  Logger("heartbeat failed after reties ~n", []),
				  {error, Reason};
			      true ->
				  do_heart_beat(Index, Socket, HeartbeatInterval, WaitResponseTimeout, MaxRetryTime, RetryTime - 1, Logger)
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


