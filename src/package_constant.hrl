-define(HEADER_LENGTH,22).
-define(ROUTER_SECRET,"96e23c8ccd9e422ca10ef6a27c2082e1").
-define(AUTH_RESPONSE_LENGTH,3 + ?HEADER_LENGTH).
-define(HEART_BEAT_RESPONSE_LENGTH,?HEADER_LENGTH).
-define(ROUTER_SHAKE_COMMAND_REQUEST,100).
-define(ROUTER_SHAKE_COMMAND_RESPONSE,101).
-define(ROUTER_HEART_BEAT_COMMAND_REQUEST,107).
-define(ROUTER_HEART_BEAT_COMMAND_RESPONSE,108).
-define(USER_ACTION_REQ,117). %% 用户上线请求与应答都是同一个command
-define(USER_ALLOW_REQ,118).
-define(USER_ALLOW_RESP,119).

-record(phead, {
	  flag = 16#5aa5,
%	  pack_len = 0,
	  device_type = 1,
	  version = 1,
	  command = 0,
	  encrypt_type = 1,
	  crc = 1,
	  pad = 1,
	  factory_id = 1009
}).

-record(authBody, {
	  timestamp,
	  routerSeq,
	  routerMac,
	  sign,
	  speed = 20,
	  routerVersion = "version 1.0",
	  rzServer = 1,
	  routerType = "x1"
}).
-record(heartbeatBody, { onlineUserNum = 0 }). 
