-module(router_farm_farm).
-export([start/1]).

start([]) -> ok;
start([Info|InfoList]) ->
    {Prefix,ClientNum, Host,Port,BindIp, StartInterval, StartBatchSize,HeartbeatInterval,WaitResponseTimeout} = Info,
    spawn(fun() ->
		  router_farm:start(Prefix,ClientNum,Host,Port,BindIp,StartInterval,StartBatchSize,HeartbeatInterval,WaitResponseTimeout)
	  end),
    start(InfoList).
    

