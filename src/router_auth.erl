-module(router_auth).
-export([do_auth/2]).
-spec do_auth(pid(),
	      {Prefix :: string() , 
	       Index :: integer(), 
	       WaitTimeout :: integer(), 
	       Retry :: integer(), 
	       Logger :: fun()}
	     ) -> ok | {error, Reason :: any()}.

do_auth(IOPid, {Prefix, Index, WaitTimeout,Retry, Logger}) ->
    Packet = packet_farm:build_package(auth, Prefix, Index,[]),
    Start = lib_misc:get_timestamp_micro_seconds(),
    case IOPid ! {self(), auth, Packet} of
	{error, Reason} ->
	    Logger(error, "send auth package failed : ~p ~n", [Reason]),
	    if
		Retry == 1 ->
		    Logger(error, "send auth failed after retries~n", []),
		    {error, Reason};
		true ->
		    Logger(error, "retry send auth : ~p~n", [Retry]),
		    do_auth(IOPid, {Prefix, Index, WaitTimeout, Retry - 1, Logger})
	    end;
	ok ->
	    receive
		{response, Response} ->
		    case check_auth(Response) of
			ok ->
			    End = lib_misc:get_timestamp_micro_seconds(),
			    Logger(info, "receive auth response valid, auth_rt=~p~n", [End - Start]),
			    ok;
			{error,Reason} -> 
			    Logger(error, "receive auth response invalid, ~p~n",[Reason]),
			    {error, Reason}
		    end
	    after WaitTimeout ->
		    Logger(error, "receive auth failed : timeout ~n", []),
		    {error, timeout}
	    end
    end.

%% 检查认证响应的合法性
-spec check_auth(Response :: binary()) -> ok | {error, Reason :: string() }.
check_auth(Response) ->
    {ok, {Success, Code}} = packet_farm:parse_package(auth, Response),
    if 
	Success == 1 -> ok;
	true -> {error, "code is " ++ integer_to_list(Code) }
    end.
