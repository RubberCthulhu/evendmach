
-module(vm_logger_event).

%% API
-export([start/0, stop/0,
	 trace/1, trace/2,
	 debug/1, debug/2,
	 info/1, info/2,
	 warn/1, warn/2,
	 err/1, err/2,
	 fatal/1, fatal/2]).

%% Debug
-export([fdatetime/0, write_msg/4]).

%% Callback
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_event).

-define(LOGGER, error_logger).

-record(state, {}).

start() ->
    error_logger:add_report_handler(?MODULE).

stop() ->
    error_logger:delete_report_handler(?MODULE).

init([]) ->
    {ok, #state{}}.

handle_event({trace_msg, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("TRACE", Pid, Format, Data),
    {ok, State};
handle_event({debug_msg, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("DEBUG", Pid, Format, Data),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("INFO", Pid, Format, Data),
    {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("WARNING", Pid, Format, Data),
    {ok, State};
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("ERROR", Pid, Format, Data),
    {ok, State};
handle_event({fatal_msg, _Gleader, {Pid, Format, Data}}, State) ->
    write_msg("FATAL", Pid, Format, Data),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

trace(Format) ->
    trace(Format, []).

trace(Format, Data) ->
    logger_notify(trace_msg, Format, Data).

debug(Format) ->
    debug(Format, []).

debug(Format, Data) ->
    logger_notify(debug_msg, Format, Data).

info(Format) ->
    info(Format, []).

info(Format, Data) ->
    logger_notify(info_msg, Format, Data).

warn(Format) ->
    warn(Format, []).

warn(Format, Data) ->
    logger_notify(warning_msg, Format, Data).

err(Format) ->
    err(Format, []).

err(Format, Data) ->
    logger_notify(error, Format, Data).

fatal(Format) ->
    fatal(Format, []).

fatal(Format, Data) ->
    logger_notify(fatal_msg, Format, Data).

logger_notify(MsgType, Format, Data) ->
    gen_event:notify(?LOGGER, {MsgType, group_leader(), {self(), Format, Data}}).

write_msg(Level, Pid, Format, Data) ->
    io:fwrite("~s ~-10s ~-16w ~s~n", 
	      [fdatetime(), Level, Pid, io_lib:format(Format, Data)]).

fdatetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Min, Sec]).











