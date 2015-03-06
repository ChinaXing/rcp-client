-define(HEADER_LENGTH,24).
-define(AUTH_RESPONSE_LENGTH,3 + ?HEADER_LENGTH).
-define(HEART_BEAT_RESPONSE_LENGTH,?HEADER_LENGTH).
-define(ROUTER_SHAKE_COMMAND_REQUEST,100).
-define(ROUTER_SHAKE_COMMAND_RESPONSE,101).
-define(ROUTER_HEART_BEAT_COMMAND_REQUEST,107).
-define(ROUTER_HEART_BEAT_COMMAND_RESPONSE,108).

-record(phead, {
	  flag = 16#5aa5,
	  pack_len = 0,
	  device_type = 1,
	  version = 1,
	  command = 0,
	  encrypt_type = 1,
	  crc = 1,
	  pad = 1,
	  factory_id = 1001
}).

-record(authBody, {
	  timestamp,
	  routerSeq,
	  routerMac,
	  sign,
	  speed = 20,
	  routerVersion = "version 1.0",
	  rzServer = 1,
	  routerType = "1"
}).
-record(heartbeatBody, { onlineUserNum = 0 }). 
