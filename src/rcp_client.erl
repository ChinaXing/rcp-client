%%%-------------------------------------------------------------------
%%% @author lenovo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2015 18:22
%%%-------------------------------------------------------------------
-module(rcp_client).
-author("lenovo").
-include_lib("stdlib/include/qlc.hrl").
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0,
  start_routers/0,
  set_start_param/1,
  get_start_param/0,
  get_router_pid_list/0,
  user_online/3,
  help/0
]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {start_args}).

help() ->
    lager:info("hello,help~n",[]),
    do_help().

do_help() ->
    lager:info("do help~n",[]),
    ok.

-type router_start_option() :: {rcp_host, Host :: string()}
| {rcp_port, Port :: integer()}
| {parallelism, Par :: integer()}
| {user_count, Count :: integer()}
| {prefix, Prefix :: string()}
| {timeout, Timeout :: integer()}
| {heartbeat_interval, Interval :: integer()}
| {client_count, Count :: integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts routers
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_routers() -> ok).
start_routers() ->
  gen_server:cast(
    ?SERVER,
    start_routers
   ).

%%--------------------------------------------------------------------
%% @doc
%% get started router's pid as list
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_router_pid_list() -> Res :: [pid()]).
get_router_pid_list() ->
  gen_server:call(
    ?SERVER,
    get_router_pid_list, 10000).
%%--------------------------------------------------------------------
%% @doc
%% do user online
%%
%% @end
%%--------------------------------------------------------------------
-spec(user_online(Count :: integer(), Offset :: integer(), PidList :: [pid()]) -> ok | {error, Reason :: term()}).
user_online(Count, Offset, PidList) ->
  gen_server:cast(
    ?SERVER,
    {do_user_online, {Count, Offset, PidList}}
  ).
%%--------------------------------------------------------------------
%% @doc
%% set router starts's parameters
%%
%% @end
%%--------------------------------------------------------------------
-spec(set_start_param(Options :: [router_start_option()]) -> ok).
set_start_param(Options) ->
  gen_server:call(
    ?SERVER,
    {set_start_param, Options}
  ).

%%--------------------------------------------------------------------
%% @doc
%% get router starts's parameters
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_start_param() -> Options :: [router_start_option()]).
get_start_param() ->
  gen_server:call(
    ?SERVER,
    get_start_param
  ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, StartArgs} = application:get_env(default_start_args),
  {ok, #state{start_args = maps:from_list(StartArgs)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({set_start_param, Options}, _From, #state{start_args = StartArgs} = State) ->
  NM = maps:from_list(Options),
  NArgs = maps:merge(StartArgs, NM),
  {reply,
    NArgs,
    State#state{start_args = NArgs}
  };

handle_call(get_start_param, _From, #state{ start_args = StartArgs} = State) ->
  {reply, StartArgs, State};

handle_call(get_router_pid_list, _From, #state{start_args = StartArgs} = State) ->
  io:format("get_router_pid_list .. ~n",[]),  
  TableName = maps:get(table_name, StartArgs),
  Rt = qlc:e(qlc:q([K || {K, _} <- ets:table(TableName)])),
  io:format("end ~n",[]),  
  {reply, Rt, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(start_routers, #state{start_args = StartArgs}) ->
  lager:info("start routers ...,~n", []),
  Prefix = maps:get(prefix, StartArgs),
  {TableName, NewArgs} = case maps:find(table_name, StartArgs) of
                           {ok, T} ->
                             {T, StartArgs};
                           error ->
                             T = list_to_atom("routers_" ++ Prefix),
                             NS = maps:put(table_name, T, StartArgs),
                             ets:new(T, [named_table]),
                             {T, NS}
                         end,
  ClientNum = maps:get(client_count, NewArgs),
  do_start_routers(Prefix, 0, ClientNum, NewArgs),
  lager:info("All clients started : ~p~n", [ClientNum]),
  spawn_link(fun() ->
    monitor_client(TableName, ClientNum, 0)
  end),
  {noreply, #state{start_args = NewArgs}};

handle_cast({do_user_online, {Count, Offset, PidList}}, State) ->
  do_user_online_loop(Count, Offset, PidList, erlang:length(PidList)),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_user_online_loop(_count, _offset, [], N) -> wait_user_online_done(N);
do_user_online_loop(Count, Offset, [Pid | Tail], N) ->
  Pid ! {self(), user_online, Offset, Count},
  do_user_online_loop(Count, Offset, Tail, N).

wait_user_online_done(0) ->
  error_logger:info_msg("wait user_online Finished~n", []),
  ok;
wait_user_online_done(N) ->
  error_logger:info_msg("wait user_online done ...~n", []),
  receive
    {From, {error, Reason}} ->
      lager:error("~p~n", [{From, {error, Reason}}]),
      wait_user_online_done(N - 1);
    {_, ok} ->
      lager:info("ok~n", []),
      wait_user_online_done(N - 1)
  end.

monitor_client(T, AliveCount, DiedCount) ->
  case ets:first(T) of
    '$end_of_table' ->
      lager:info("All clients shutdown!~n", []);
    _ ->
      receive
        {'DOWN', _, process, Pid, Info} ->
          ets:delete(T, Pid),
          lager:error("Router exit message : ~p~n", [Info]),
          lager:error("Router : alive = ~p, diede = ~p ~n", [AliveCount - 1, DiedCount + 1]),
          monitor_client(T, AliveCount - 1, DiedCount + 1)
      end
  end.


do_start_routers(_Prefix, FromIndex, ClientNum, _Map) when FromIndex >= ClientNum -> ok;
do_start_routers(Prefix, FromIndex, ClientNum, Map) ->
  StartBatchSize = maps:get(batch_size, Map),
  lager:info("Start : ~p -> ~p~n", [FromIndex, FromIndex + StartBatchSize]),
  do_parallel_start_routers(Prefix, FromIndex, min(FromIndex + StartBatchSize, ClientNum), Map),
  lib_misc:sleep(1000),
  do_start_routers(Prefix, FromIndex + StartBatchSize, ClientNum, Map).

do_parallel_start_routers(_, FromIndex, FromIndex, _Map) -> ok;
do_parallel_start_routers(Prefix, FromIndex, ToIndex, Map) ->
  Prefix = maps:get(prefix, Map),
  UserCount = maps:get(user_count, Map),
  Host = maps:get(rcp_host, Map),
  Port = maps:get(rcp_port, Map),
  Timeout = maps:get(timeout, Map),
  HeartBeatInterval = maps:get(heartbeat_interval, Map),
  {Pid, _} = spawn_monitor(router, start, [Prefix, FromIndex, UserCount, Host, Port, HeartBeatInterval, Timeout]),
  register(list_to_atom("router_io_" ++ Prefix ++ integer_to_list(FromIndex)), Pid),
  PidTable = maps:get(table_name, Map),
  ets:insert(PidTable, {Pid, FromIndex}),
  do_parallel_start_routers(Prefix, FromIndex + 1, ToIndex, Map).
