-module(router_user_online).
-compile(export_all).

do_online(_, {0, _, _, _, _}) -> ok;
do_online(IOPid, {Count, Prefix, RouterIndex, WaitTimeout, Logger}) ->
    Logger(info, "do user online : ~p, ~p, ~p~n", [Prefix, RouterIndex, Count]),
    Pkg = packet_farm:build_package(user_online, RouterIndex, Count,[]),
    Start = lib_misc:get_timestamp_micro_seconds(),
    IOPid ! {self(), user_online, Pkg},
    receive
	{error, Reason} ->
	    Logger(error, "send user online failed : ~p~n",[Reason]),
	    {error, Reason};
	ok ->
	    Logger(info, "send user_online request ok~n",[]), 
	    receive
		{error, Reason} ->
		    Logger(error, "receive user_online response error : ~p~n",[Reason]),
		    {error, Reason};
		{response, Response} ->
		    case check_online(Response) of
			{ok, Result} ->
			    End = lib_misc:get_timestamp_micro_seconds(),
			    Logger(info, "receive user_online response valid:~p, online_rt=~p~n", [Result, End - Start]),
			    lib_misc:sleep(1000),
			    do_online(IOPid, {Count - 1, Prefix, RouterIndex, WaitTimeout, Logger});
			{error, Reason} ->
			    Logger(error, "receive user_online response invalid:~p ~n",[ Reason]),
			    {error, Reason}
		    end
	    after WaitTimeout ->
		    Logger(error, "receive user_online failed : timeout~n",[]),
		    {error, recv_resp_timeout}
	    end
    after WaitTimeout ->
	    Logger(error, "send user_online request failed : timeout ~n", []),
	    {error, send_req_timeout}
    end.
		
check_online(Response) ->
    packet_farm:parse_package(user_allow, Response).
    
