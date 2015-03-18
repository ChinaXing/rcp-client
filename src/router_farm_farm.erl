-module(router_farm_farm).
-export([start/1]).

start(List) -> start0(List, 0).

start0([], _ ) -> ok;
start0([Info|InfoList], Index) ->
    {Prefix,ClientNum, Host,Port,BindIp, StartInterval, StartBatchSize,HeartbeatInterval,WaitResponseTimeout} = Info,
    spawn(fun() ->
		  router_farm:start(Index, Prefix,ClientNum,Host,Port,BindIp,StartInterval,StartBatchSize,HeartbeatInterval,WaitResponseTimeout)
	  end),
    start0(InfoList, Index + 1).
    

