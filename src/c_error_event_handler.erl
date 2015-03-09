-module(c_error_event_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,terminate/2, code_change/3, handle_call/2, handle_info/2]).
init(InitArgs) ->
    [LogFile|_] = InitArgs,
    case file:open(LogFile,write) of
	{ok, IoDevice} ->
	    {ok, {io, IoDevice}};
	{error,Reason} ->
	    {error, Reason}
    end.

handle_event(Event, State) ->
    case Event of
	{error, _, {_, Format, Data}} ->
	    {io, IoDevice} = State,
	    io:format(IoDevice,"[ERROR] " ++ Format,Data),
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
			  
