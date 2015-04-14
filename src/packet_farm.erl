-module(packet_farm).
%-export([build_package/4, parse_package/2, get_packet_command/1]).
-compile(export_all).
-include("package_constant.hrl").

build_package(user_online, RouterIndex , Index, []) ->
    Operation = 1,
    IsIpV6 = 0,
    UserIp = lib_misc:get_ip(192, 168, RouterIndex, Index),
    UserMac = lib_misc:get_mac(RouterIndex bsl 8 + Index),
    ApMac = lib_misc:get_mac(RouterIndex bsl 8),
    Ssid = integer_to_list(RouterIndex) ++ "_" ++ integer_to_list(Index),
    IsWired = 0,
    IsEncryption = 0,
    Ssid0 = list_to_binary(string:left(Ssid,32,0)),
    PB = <<Operation:8,
	   IsIpV6:8,
	   UserIp/binary,
	   0:96,
	   UserMac/binary,
	   0:16,
	   ApMac/binary,
	   0:16,
	   Ssid0/binary,
	   IsWired:8,
	   IsEncryption:8
	 >>,
    PH = build_header(?USER_ACTION_REQ),
    list_to_binary([PH,PB]);


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
    ok;

parse_package(user_allow, Bin) ->
    <<_:(?HEADER_LENGTH*8), Cmd:8, _/binary>> = Bin,    
    case Cmd of
	1 ->
	    {ok, pass};
	2 ->
	    {ok, forbidden};
	3 ->
	    {ok, stop_and_jump};
	4 ->
	    {ok, pass_and_jump};
	_ ->
	    {error, Cmd}
    end.
	    
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

	    
	    
get_packet_command_req(ResponseCmd) ->
    case ResponseCmd of
	user_allow -> {ok, [user_online, user_offline]};
	heartbeat  -> {ok, [heartbeat]};
	auth -> {ok, [auth]};
	_ -> {unknow, ResponseCmd}
    end.
	    
