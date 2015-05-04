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
-compile([{parse_transform, lager_transform}]).

%% API
-compile(export_all).
-record(mac, {a = 0, b = 0, c = 0, d = 0, e = 0, f = 0}).

sleep(Timeout) ->
    receive
    after Timeout -> ok
    end.

do_log(Message, Args) ->
    lager:start(),
    lager:info(Message, Args).

lib_init(Index) ->
    {_, B, C} = now(),
    random:seed(B, C, Index).

sleep_random(Timeout, Delta) ->
    sleep(Timeout - Delta + random:uniform(2 * Delta)).

list_flatten(L) -> list_reverse((list_flatten_loop([], L))).

list_flatten_loop(Result, []) -> Result;
list_flatten_loop(Result, [T | H]) ->
    list_flatten_loop(list_concat(Result, T), H).

list_concat(A, B) ->
    list_concat0(lists:reverse(A), B).

list_concat0([], B) -> B;
list_concat0([I | A], B) ->
    list_concat0(A, [I | B]).

list_reverse(L) -> list_reverse_loop([], L).
list_reverse_loop(L, []) -> L;
list_reverse_loop(L, [I | H]) -> list_reverse_loop([I | L], H).

get_logger(Identity, Args0) when is_list(Identity) ->
    F = fun({Context, Type}, Strformat, Args) ->
		case Type of
		    error ->
			if 
			    Context =:=  undefined ->
				lager:error(Identity ++ Strformat, list_concat(Args0, Args));
			    true ->
				lager:error(Context, Identity ++ Strformat, list_concat(Args0, Args))
			end;
		    info ->
			if 
			    Context =:=  undefined ->
				lager:info(Identity ++ Strformat, list_concat(Args0, Args));
			    true ->
				lager:info(Context, Identity ++ Strformat, list_concat(Args0, Args))
			end;
		    warn ->
			if 
			    Context =:=  undefined ->
				lager:warning(Identity ++ Strformat, list_concat(Args0, Args));
			    true ->
				lager:warning(Context, Identity ++ Strformat, list_concat(Args0, Args))
			end;
		    _ ->
			if 
			    Context =:=  undefined ->
				lager:info(Identity ++ Strformat, list_concat(Args0, Args));
			    true ->
				lager:info(Context, Identity ++ Strformat, list_concat(Args0, Args))
			end
		end
	end,
    fun(Attr, Strformat, Args) ->
	    case Attr of
		{Context, Type} ->
		    F({Context, Type}, Strformat, Args);
		_ ->
		    F({undefined, Attr}, Strformat, Args)
	    end
    end.

get_logger(File) ->
    {ok, IoDevice} = file:open(File, write),
    fun(Format, Args) ->
	    io:format(IoDevice, Format, Args)
    end.


get_timestamp_seconds() ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B.

get_timestamp_micro_seconds() ->
    {A, B, C} = os:timestamp(),
    (A * 1000000 + B) * 1000000 + C.

get_timestamp_million_seconds() ->
    {A, B, C} = os:timestamp(),
    (A * 1000000 + B) * 1000 + round(C / 1000).

get_mac(Index) ->
    M = #mac{a = Index band 255, b = (Index bsr 8) band 255,
	     c = (Index bsr 16) band 255, d = (Index bsr 24) band 255},
    <<(M#mac.a), (M#mac.b), (M#mac.c), (M#mac.d), (M#mac.e), (M#mac.f)>>.

get_sign(RouterSeq, Secret, Time) ->
    Md50 = get_md5_hex_str(Secret ++ RouterSeq),
    get_md5_hex_str(Md50 ++ integer_to_list(Time)).


get_md5_hex_str(Str) ->
    X = erlang:md5(Str),
    [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= X].

get_ip(Index0, Index1, Index2, Index3) ->
    <<Index0:8, Index1:8, Index2:8, Index3:8>>.

gc_repeater(stop) ->
    case get(gc_repeater) of
	Pid when is_pid(Pid) ->
	    Pid ! {self() ,stop};
	_ ->
	    not_exsit
    end;

gc_repeater(start) ->
    Interval = case get(gc_repeater_interval) of
		   undefined ->
		       io:format("Cannot found gc_repeater_interval, use 60 seconds~n" ,[]),
		       60;
		   I when is_integer(I) ->
		       I;
		   _ ->
		       io:format("invalid gc_repeater_interval, not a number , use 60 seconds~n" ,[]),
		       60
	       end,
    Pid = spawn(fun() -> gc_loop0(Interval) end),
    put(gc_repeater, Pid);

gc_repeater(Interval) when is_integer(Interval)->
    put(gc_repeater_interval, Interval).

gc_loop0(Interval) ->
    io:format("do gc ...~n",[]),
    [ erlang:garbage_collect(Pid) || Pid <- erlang:processes() ],
    receive 
	{_From, stop} ->
	    io:format("stop gc, as you wanted !~n",[]),
	    ok
    after (Interval * 1000) ->
	    gc_loop0(Interval)
    end.

mac_to_string(Mac) ->
    <<M:48,_/binary>> = <<Mac:64>>,
    L = [ I || <<I:4>> <= <<M:48>> ],
    {_, R } = lists:foldl(fun(Elem, {C,Acc}) ->
				  if 
				      C =:= 0  ->
					  {C+1, Acc ++ to_hex_string(Elem)};
				      C rem 2 =:= 0 ->
					  {C+1, Acc ++ ":" ++ to_hex_string(Elem)};
				      true ->
					  {C+1, Acc ++ to_hex_string(Elem)}
				  end
			  end, {0, []}, L),
    R.


to_hex_string(E) when E > 9 ->
    [$A + (E-10)];
to_hex_string(E) ->
    [$0 + E].
