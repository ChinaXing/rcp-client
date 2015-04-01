-module(packet_farm).
-export([build_package/4, parse_package/2, get_packet_command/1]).
-include("package_constant.hrl").

build_package(auth, Prefix, Index,[]) ->
    RouterSeq_0 = "r_" ++ Prefix ++ integer_to_list(Index),
    RouterSeq = string:left(RouterSeq_0, 64, 0),
    Timestamp = lib_misc:get_timestamp_million_seconds(),
    PB = #authBody{
	    timestamp = Timestamp,
	    routerSeq = RouterSeq,
	    routerMac = lib_misc:get_mac(Index),
	    sign = string:left(lib_misc:get_sign(RouterSeq_0, ?ROUTER_SECRET, Timestamp),32,0)
	   },
    PBbits = list_to_binary([<<(PB#authBody.timestamp):64>>,
			     list_to_binary(PB#authBody.routerSeq),
			     PB#authBody.routerMac,<<0,0>>,
			     list_to_binary(PB#authBody.sign),
			     <<(PB#authBody.speed):16>>,
			     <<(length(PB#authBody.routerVersion)):16>>,
			     list_to_binary(PB#authBody.routerVersion),
			     <<(PB#authBody.rzServer)>>,
			     <<(length(PB#authBody.routerType)):16>>,
			     list_to_binary(PB#authBody.routerType)
	     ]),
    % PkgLen = byte_size(PBbits) + ?HEADER_LENGTH,
    PH = build_header(?ROUTER_SHAKE_COMMAND_REQUEST),
    list_to_binary([PH,PBbits]);

build_package(heartbeat, _, _, [OnlineNum]) ->
    PB = #heartbeatBody{ onlineUserNum = OnlineNum},
    PBbits = <<(PB#heartbeatBody.onlineUserNum):16>>,
%    PkgLen = byte_size(PBbits) + 24,
    PH = build_header(?ROUTER_HEART_BEAT_COMMAND_REQUEST),
    list_to_binary([PH,PBbits]).

build_header(Command) ->
    P = #phead{command = Command},
    <<(P#phead.flag):16,
      (P#phead.device_type):16,
      (P#phead.version):16,
      (P#phead.command):16,
      (P#phead.encrypt_type):16,
      (P#phead.crc):32,
      (P#phead.pad):32,
      (P#phead.factory_id):32>>.

parse_package(auth, Bin) ->
    <<_:(?HEADER_LENGTH*8), Success,Code:16>> = Bin,
    {ok, {Success, Code}};
parse_package(heartbeat, _ ) ->
    ok.
    
get_packet_command(Packet) ->
    try	
	<<_:48,Command:16,_/binary>> = Packet,
	case Command of
	    ?ROUTER_SHAKE_COMMAND_RESPONSE ->
		auth;
	    ?ROUTER_HEART_BEAT_COMMAND_RESPONSE ->
		heartbeat;
	    ?USER_ALLOW_REQ ->
		user_allow;
	    _ ->
		{unknow, {command, Command}}
	end
    catch
	_:_ -> {unknow, {command, parse_error}}
    end.

	    
	    
