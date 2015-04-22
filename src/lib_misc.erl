%%%-------------------------------------------------------------------
%%% @author LambdaCat
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 三月 2015 下午4:43
%%%-------------------------------------------------------------------
-module(lib_misc).
-author("LambdaCat").

%% API
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-record(mac , { a=0, b=0, c=0, d=0, e=0, f=0 }).

sleep(Timeout) ->
  receive
  after Timeout -> ok
  end.

do_log(Message,Args) ->
    lager:start(),
    lager:info(Message,Args).

lib_init(Index) ->
    {_,B,C} = now(),
    random:seed(B,C,Index).

sleep_random(Timeout, Delta) ->
    sleep(Timeout - Delta + random:uniform(2 * Delta)).

list_flatten(L) -> list_reverse((list_flatten_loop([], L))).

list_flatten_loop(Result, []) -> Result;
list_flatten_loop(Result, [T | H]) ->
  list_flatten_loop(list_concat(Result, T), H).

list_concat(A, B) ->
    list_concat0(lists:reverse(A), B).

list_concat0([], B) -> B;
list_concat0([I|A], B) ->
  list_concat0(A, [I|B]).

list_reverse(L) -> list_reverse_loop([], L).
list_reverse_loop(L, []) -> L;
list_reverse_loop(L, [I | H]) -> list_reverse_loop([I | L], H).

gen_logger(Identity, Args0) -> 
    fun(Type, Strformat, Args) -> 
	    case Type  of
		error ->
		    lager:error(Identity ++ Strformat, list_concat(Args0, Args));
		info ->
		    lager:info(Identity ++ Strformat, list_concat(Args0, Args));
		warn ->
		    lager:warning(Identity ++ Strformat, list_concat(Args0, Args));
		_ -> 
		    lager:info(Identity ++ Strformat, list_concat(Args0, Args))
	    end
    end.

get_logger(File) ->
    {ok, IoDevice} = file:open(File,write),
    fun(Format, Args) ->
		   io:format(IoDevice,Format,Args)
    end.
    

get_timestamp_seconds() ->
    {A,B,_} = os:timestamp(),
    A * 1000000 + B.

get_timestamp_micro_seconds() ->
    {A, B, C} = os:timestamp(),
    (A * 1000000 + B) * 1000000 + C.

get_timestamp_million_seconds() ->
    {A, B, C} = os:timestamp(),
    (A * 1000000 + B) * 1000 + round(C/1000).

get_mac(Index) ->
    M=#mac{a=Index band 255, b = (Index bsr 8) band 255, 
	       c = (Index bsr 16) band 255, d = (Index bsr 24) band 255},
    <<(M#mac.a),(M#mac.b),(M#mac.c),(M#mac.d),(M#mac.e),(M#mac.f)>>.

get_sign(RouterSeq,Secret,Time) ->
    Md50 = get_md5_hex_str(Secret ++ RouterSeq),
    get_md5_hex_str(Md50 ++ integer_to_list(Time)).
    

get_md5_hex_str(Str) ->
    X = erlang:md5(Str),
    [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= X].

get_ip(Index0, Index1, Index2, Index3) ->
    <<Index0:8,Index1:8,Index2:8,Index3:8>>.


    
