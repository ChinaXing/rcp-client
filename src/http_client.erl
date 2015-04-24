-module(http_client).
-compile(export_all).

-define(ALLOW_USER_URL, "http://127.0.0.1:8080/rest/allowUserRequest").

start(user_allow, Parallelism, Count) ->
  hackney:start(),
  Logger = lib_misc:get_logger(""),
  SeqLoop = fun(ThreadNum) ->
    do_loop(
      Count,
      fun(SequenceNum) ->
        Token = "" ++ ThreadNum,
        MerchantId = "" ++ SequenceNum,
        Oauth = "",
        OauthInfo = "",
        UserOs = "",
        AllowSeconds = "",
        do_allow_user(Logger, Token, MerchantId, Oauth, OauthInfo, UserOs, AllowSeconds)
      end)
  end,
  ets:new(client_user_allow, [named_table]),
  do_loop(Parallelism,
    fun(ThreadNum) ->
      {Pid, _} = spawn_monitor(fun() ->
        SeqLoop(ThreadNum)
      end),
      ets:insert(client_user_allow, [{Pid, ThreadNum}]),
      ok
    end),
  mon_thread(Logger).

mon_thread(Logger) ->
  case ets:first(client_user_allow) of
    '$end_of_table' ->
      error_logger:info_msg("All thread exit ~n", []);
    _ ->
      receive
        {'DOWN', _, Pid} ->
          ThreadNum = ets:get(client_user_allow, Pid),
          ets:delete(client_user_allow, Pid),
          Logger(info, "Pid exit : ~p,~p~n", [Pid, ThreadNum]),
          mon_thread(Logger)
      end
  end.

do_loop(0, _Fun) -> ok;
do_loop(N, Fun) ->
  case Fun(N) of
    ok ->
      do_loop(N - 1, Fun);
    {error, Reason} ->
      {erroor, Reason}
  end.

%do_push_data({}) -> 

%% @RequestMapping("/rest/allowUserRequest")
%%     public String allowUserRequest(HttpServletRequest request, HttpServletResponse response, ModelMap model,
%%                                    String token, String merchantId, String oauth, String oauthInfo, String userOS,
%%                                    Integer allowSeconds)

do_allow_user(Logger, Token, MerchantId, Oauth, OauthInfo, UserOs, AllowSeconds) ->
  case hackney:request(
    ?ALLOW_USER_URL,
    [{<<"user-agent">>, <<"CHinaXing">>}],
    {form, [
      {token, Token},
      {merchantId, MerchantId},
      {oauth, Oauth},
      {oauthInfo, OauthInfo},
      {userOs, UserOs},
      {allowSeconds, AllowSeconds}
    ]
    },
    [{connect_timeout, 3000}, {recv_timeout, 3000}]
  ) of
    {ok, ResponseStatus, _, Ref} ->
      case ResponseStatus of
        200 ->
          {ok, Body} = hackney:body(Ref),
          case check_allow_user_body(Logger, Body) of
            ok ->
              Logger(info, "do allow_user succeed~n", []),
              ok;
            {error, Reason} ->
              Logger(error, "do allow_user failed:~p ~n", [Reason])
          end;
        _ ->
          Logger(error, "do allow_user failed, response status not 200:~p~n", [ResponseStatus]),
          {error, status_code_not_200}
      end;
    {error, Reason} ->
      Logger(error, "do allow_user failed, send request failed: ~p~n", [Reason]),
      {error, Reason}
  end.

% TODO		    
check_allow_user_body(Logger, Body) ->
  Logger(info, "~p~n", [Body]),
  ok.
    
			    
		
				
    
			     
