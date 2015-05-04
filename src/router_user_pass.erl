-module(router_user_pass).
-export([start/2]).

start(IOPid, Logger) ->
    Pid = spawn_link(fun() ->
		       Logger(info, "start event loop~n",[]),
		       start_event_loop(IOPid, Logger)
	       end),
    IOPid ! {self(), set_packet_processor, {user_pass, Pid}}, 
    {ok, Pid}.

start_event_loop(IOPid, Logger) ->
    receive 
	{user_pass, data, Data} -> 
	    Mac = get_user_mac(Data),
	    MacString = lib_misc:mac_to_string(Mac),
	    Logger(info, "receive user_pass_request, Mac : ~p~n",[MacString]),
	    Resp = packet_farm:build_package(user_pass_resp, undefined, undefined, [Mac]),
	    IOPid ! {self(), user_pass_resp, Resp, MacString},
	    start_event_loop(IOPid, Logger);
	{user_pass_resp, send_result, Res, MacString} ->
	    case Res of
		ok -> 
		    Logger(info, "send user_pass_resp ok, Mac : ~p~n",[MacString]);
		{error, Reason} ->
		    Logger(error, "send user_pass_resp failed : ~p, Mac : ~p~n", [Reason, MacString])
	    end,
	    start_event_loop(IOPid, Logger)
    end.

get_user_mac(Data) ->
    Body = packet_farm:parse_body(user_allow,Data),
    <<_cmd:8,_isIpV6:8,_userIp:128,UserMac:64,_/binary>> = Body,
    UserMac.
