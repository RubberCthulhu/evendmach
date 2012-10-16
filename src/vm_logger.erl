
-module(vm_logger).

-behaviour(gen_server).

-compile({no_auto_import, [error/2]}).

%% API
-export([start_link/0, start_link/1, start/0, start/1]).

%% Logging API
-export([trace/1, trace/2,
	 debug/1, debug/2,
	 info/1, info/2,
	 warn/1, warn/2,
	 error/1, error/2,
	 fatal/1, fatal/2,
	 set_level/1
	]).

%% Callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Debug
-export([msg/3, write/6, fdatetime/0]).

-define(SERVER, ?MODULE).

-define(LVL_TRACE, 100).
-define(LVL_DEBUG, 90).
-define(LVL_INFO, 80).
-define(LVL_WARN, 70).
-define(LVL_ERROR, 60).
-define(LVL_FATAL, 50).

-record(state, {
	  level,
	  stdin,
	  stderr,
	  logfile,
	  logmode,
	  fd
}).

start() ->
    start([]).

start(Args) ->
    gen_server:start({local, vm_logger}, ?MODULE, Args, []).

%stop() ->
%    error_logger:delete_report_handler(?MODULE).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    DefaultState = #state{
      level = trace,
      stdin = true,
      stderr = false,
      logfile = undefined,
      logmode = append,
      fd = undefined
     },

    case conf(DefaultState, Args) of
	{ok, State} ->
	    case prepare_output(State) of
		{ok, NewState} ->
		    {ok, NewState};
		{error, Reason} ->
		    {stop, {error, Reason}}
	    end;
	{error, Reason} ->
	    {stop, {error, Reason}}
    end.

conf(State, []) ->
    {ok, State};
conf(State, [Arg | Rest]) ->
    case conf_arg(State, Arg) of
	{ok, NewState} ->
	    conf(NewState, Rest);
	{error, Reason} ->
	    {error, Reason}
    end;
conf(_, _) ->
    {error, invalid_args}.

conf_arg(State, {level, Level}) when (Level =:= trace) or
				     (Level =:= debug) or 
				     (Level =:= info) or 
				     (Level =:= warn) or
				     (Level =:= error) or
				     (Level =:= fatal) ->
    {ok, State#state{level = Level}};
conf_arg(State, {stdin, Flag}) when is_boolean(Flag) ->
    {ok, State#state{stdin = Flag}};
conf_arg(State, {stderr, Flag}) when is_boolean(Flag) ->
    {ok, State#state{stderr = Flag}};
conf_arg(State, {logfile, Name}) when is_list(Name) ->
    {ok, State#state{logfile = Name}};
conf_arg(State, {rewrite, Flag}) when is_boolean(Flag) ->
    case Flag of
	true ->
	    {ok, State#state{logmode = write}};
	false ->
	    {ok, State#state{logmode = append}}
    end;
conf_arg(_, _) ->
    {error, invalid_arg}.

prepare_output(#state{logfile = undefined} = State) ->
    {ok, State};
prepare_output(#state{logfile = File, logmode = Mode} = State) ->
    case file:open(File, [Mode]) of
	{ok, Fd} ->
	    {ok, State#state{fd = Fd}};
	{error, Reason} ->
	    {error, Reason}
    end.

%log_fds(#state{stdin = Stdin, stderr = Stderr, fd = Fd}) ->

handle_cast({trace, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_TRACE, "TRACE", Pid, Format, Data),
    {noreply, State};
handle_cast({debug, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_DEBUG, "DEBUG", Pid, Format, Data),
    {noreply, State};
handle_cast({info, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_INFO, "INFO", Pid, Format, Data),
    {noreply, State};
handle_cast({warn, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_WARN, "WARNING", Pid, Format, Data),
    {noreply, State};
handle_cast({error, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_ERROR, "ERROR", Pid, Format, Data),
    {noreply, State};
handle_cast({fatal, _Gleader, Pid, Format, Data}, State) ->
    write(State, ?LVL_FATAL, "FATAL", Pid, Format, Data),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({level, Level}, _, State) ->
    case conf(State, {level, Level}) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{fd = Fd}) when Fd =/= undefined ->
    file:close(Fd),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

set_level(Level) ->
    gen_server:call(?SERVER, {level, Level}).

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

write(#state{level = Level} = State, MsgLevel, LevelText, Pid, Format, Data) 
  when Level >= MsgLevel ->
    Fun = fun (Dev) ->
		  io:fwrite(Dev, "~s ~-10s ~-16w ~s~n", 
			    [fdatetime(), LevelText, Pid, 
			     io_lib:format(Format, Data)])
	  end,

    case State#state.stdin of
	true ->
	    Fun(standard_io);
	false ->
	    ok
    end,

    case State#state.stderr of
	true ->
	    Fun(standard_error);
	false ->
	    ok
    end,

    case State#state.fd of
	undefined ->
	    ok;
	_ ->
	    Fun(State#state.fd)
    end,

    ok;
write(_, _, _, _, _, _) ->
    ok.

fdatetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Min, Sec]).





