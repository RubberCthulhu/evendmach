
-module(vm_logger).

-behaviour(gen_server).

-compile({no_auto_import, [error/2]}).

%% API
-export([start_link/0, start/0]).

%% Logging API
-export([trace/1, trace/2,
	 debug/1, debug/2,
	 info/1, info/2,
	 warn/1, warn/2,
	 error/1, error/2,
	 fatal/1, fatal/2
	]).

%% Callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Debug
-export([msg/3, write/4, fdatetime/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

start() ->
    gen_server:start({local, vm_logger}, ?MODULE, [], []).

%stop() ->
%    error_logger:delete_report_handler(?MODULE).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}, infinity}.

handle_cast({trace, _Gleader, Pid, Format, Data}, State) ->
    write("TRACE", Pid, Format, Data),
    {noreply, State};
handle_cast({debug, _Gleader, Pid, Format, Data}, State) ->
    write("DEBUG", Pid, Format, Data),
    {noreply, State};
handle_cast({info, _Gleader, Pid, Format, Data}, State) ->
    write("INFO", Pid, Format, Data),
    {noreply, State};
handle_cast({warn, _Gleader, Pid, Format, Data}, State) ->
    write("WARNING", Pid, Format, Data),
    {noreply, State};
handle_cast({error, _Gleader, Pid, Format, Data}, State) ->
    write("ERROR", Pid, Format, Data),
    {noreply, State};
handle_cast({fatal, _Gleader, Pid, Format, Data}, State) ->
    write("FATAL", Pid, Format, Data),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {ok, {error, unknown_message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

msg(Type, Format, Data) ->
    gen_server:cast(?SERVER, {Type, group_leader(), self(), Format, Data}).

trace(Format) ->
    trace(Format, []).

trace(Format, Data) ->
    msg(trace, Format, Data).

debug(Format) ->
    debug(Format, []).

debug(Format, Data) ->
    msg(debug, Format, Data).

info(Format) ->
    info(Format, []).

info(Format, Data) ->
    msg(info, Format, Data).

warn(Format) ->
    warn(Format, []).

warn(Format, Data) ->
    msg(warn, Format, Data).

error(Format) ->
    error(Format, []).

error(Format, Data) ->
    msg(error, Format, Data).

fatal(Format) ->
    fatal(Format, []).

fatal(Format, Data) ->
    msg(fatal, Format, Data).

write(Level, Pid, Format, Data) ->
    io:fwrite("~s ~-10s ~-16w ~s~n", 
	      [fdatetime(), Level, Pid, io_lib:format(Format, Data)]).

fdatetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Min, Sec]).











