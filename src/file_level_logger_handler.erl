-module(file_level_logger_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,terminate/2, code_change/3, handle_call/2, handle_info/2]).
-type logFile() :: string ().
-type append() :: boolean().
-type level() :: info | warn | error.
-type levelOnly() :: boolean().
-type initArgType() :: { logFile(), append(), level(), levelOnly() }.
-spec init(initArgType()) -> {error, Reason :: term() } | {ok, State :: term()}.

init(InitArgs) ->
    {LogFile,Append,Level,LevelOnly} = InitArgs,
    case get_file_handler(LogFile,Append) of
	{ok, IoDevice} ->
	    {ok, {io, IoDevice, level, Level, levelOnly, LevelOnly}};
	{error,Reason} ->
	    {error, Reason}
    end.

handle_event(Event, State) ->
    {io, IoDevice, level, Level, levelOnly, LevelOnly} = State,
    {Level0, _, {_, Format, Data}} = Event,
    {{Year,Month,Day},{Hour,Minite,Second}} = erlang:localtime(),
    case {Level0, Level, LevelOnly} of
	{error, error, _ } ->
	    io:format(IoDevice,"[~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w] [ERROR] " ++ Format,[Year,Month,Day,Hour,Minite,Second|Data]),
	    {ok, State, hibernate};
	{error, _, false } ->
	    io:format(IoDevice,"[~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w] [ERROR] " ++ Format,[Year,Month,Day,Hour,Minite,Second|Data]),
	    {ok, State, hibernate};
	{warn_msg, warn, _ } ->
	    io:format(IoDevice,"[~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w] [WARN] " ++ Format,[Year,Month,Day,Hour,Minite,Second|Data]),
	    {ok, State, hibernate};
	{warn_msg, info, false} ->
	    io:format(IoDevice,"[~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w] [WARN] " ++ Format,[Year,Month,Day,Hour,Minite,Second|Data]),
	    {ok, State, hibernate};
	{info_msg, info, _ } ->
	    io:format(IoDevice,"[~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w] [INFO] " ++ Format,[Year,Month,Day,Hour,Minite,Second|Data]),
	    {ok, State, hibernate};
	 _ -> 
	    {ok, State}
    end.

terminate(_, State) ->
    {io, IoDevice} = State,
    file:close(IoDevice),
    ok.
    
code_change(_,State,_) -> {ok, State}.
handle_call(_, State) -> {ok, State}.
handle_info(_, State) -> {ok, State}.
			  
get_file_handler(File, true)  -> file:open(File,[write,append]);
get_file_handler(File, false) -> file:open(File,write).

    
    
