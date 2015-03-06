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
-record(mac , { a=0, b=0, c=0, d=0, e=0, f=0 }).

sleep(Timeout) ->
  receive
  after Timeout -> ok
  end.

list_flatten(L) -> list_reverse((list_flatten_loop([], L))).

list_flatten_loop(Result, []) -> Result;
list_flatten_loop(Result, [T | H]) ->
  list_flatten_loop(list_concat(Result, T), H).

list_concat(A, []) -> A;
list_concat(A, [I | B]) ->
  list_concat([I | A], B).

list_reverse(L) -> list_reverse_loop([], L).
list_reverse_loop(L, []) -> L;
list_reverse_loop(L, [I | H]) -> list_reverse_loop([I | L], H).

gen_logger(Identity, Args0) -> fun(Strformat, Args) -> io:format(Identity ++ Strformat, list_concat(Args0, Args)) end.

get_timestamp() ->
    {A,B,_} = os:timestamp(),
    A * 1000000 + B.


get_mac(Index) ->
    M=#mac{a=Index band 255, b = (Index bsr 8) band 255, 
	       c = (Index bsr 16) band 255, d = (Index bsr 24) band 255},
    <<(M#mac.a),(M#mac.b),(M#mac.c),(M#mac.d),(M#mac.e),(M#mac.f)>>.

get_sign(Index) ->
    "hello,world" ++ integer_to_list(Index).
