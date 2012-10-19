
-module(vm_logger).

-behaviour(gen_server).

-compile({no_auto_import, [error/2]}).

%% API
-export([start_link/0, start_link/1, start/0, start/1]).

%% Logging API
-export([
	 log/2, log/3,
	 trace/1, trace/2,
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
-export([write/5, fdatetime/0, level_filter/2]).

-define(SERVER, ?MODULE).

-record(state, {
	  level = debug :: none | trace | debug | info | warn | error | fatal,
	  stdin = true :: boolean(),
	  stderr = false :: boolean(),
	  logfile = undefined :: undefined | string(),
	  logmode = append :: write | append,
	  fd = undefined :: undefined | file:io_device(),
	  rotate = none :: none | minutely | hourly | daily | weekly | monthly | yearly,
	  tmRotate = undefined :: undefined | calendar:datetime()
}).

start() ->
    start([]).

start(Args) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).
%    gen_server:start(?MODULE, Args, []).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(Args) ->
    DefaultState = #state{},
    case conf(DefaultState, Args) of
	{ok, State} ->
	    case prepare_output(State, calendar:local_time()) of
		{ok, NewState} ->
		    {ok, NewState, calc_timeout(NewState)};
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
				     (Level =:= fatal) or
				     (Level =:= none) ->
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
conf_arg(State, {rotate, Freq}) when (Freq =:= none) or
				     (Freq =:= minutely) or
				     (Freq =:= hourly) or
				     (Freq =:= daily) or
				     (Freq =:= weekly) or
				     (Freq =:= monthly) or
				     (Freq =:= yearly) ->
    {ok, State#state{rotate = Freq}};
conf_arg(_, _) ->
    {error, invalid_arg}.

log_fds(#state{stdin = Stdin, stderr = Stderr, fd = Fd}) ->
    lists:flatten([vm_utils:ifelse(Stdin, [standard_io], []),
		   vm_utils:ifelse(Stderr, [standard_error], []),
		   vm_utils:ifelse(Fd /= undefined, [Fd], [])
		  ]).

handle_cast({Level, _Gleader, Pid, Format, Data}, State) ->
    case write(State, Level, Pid, Format, Data) of
	{ok, NewState} ->
	    {noreply, NewState, calc_timeout(NewState)};
	{error, Reason} ->
	    {stop, {error, Reason}}
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, calc_timeout(State)}.

handle_call({level, Level}, _, State) ->
    case conf(State, [{level, Level}]) of
	{ok, NewState} ->
	    {reply, ok, NewState, calc_timeout(NewState)};
	{error, Reason} ->
	    {reply, {error, Reason}, State, calc_timeout(State)}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State, calc_timeout(State)}.

handle_info(timeout, State) ->
    error_logger:info_msg("handle_info(timeout)"),
    case prepare_output(State, calendar:local_time()) of
	{ok, NewState} ->
	    {noreply, NewState, calc_timeout(NewState)};
	{error, Reason} ->
	    {stop, {error, Reason}, State}
    end;
handle_info(_Info, State) ->
    {noreply, State, calc_timeout(State)}.

terminate(_Reason, #state{fd = Fd}) when Fd =/= undefined ->
    file:close(Fd),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

set_level(Level) ->
    gen_server:call(?SERVER, {level, Level}).

log(Level, Format) ->
    log(Level, Format, []).

log(Level, Format, Data) ->
    gen_server:cast(?SERVER, {Level, group_leader(), self(), Format, Data}).

trace(Format) ->
    trace(Format, []).

trace(Format, Data) ->
    log(trace, Format, Data).

debug(Format) ->
    debug(Format, []).

debug(Format, Data) ->
    log(debug, Format, Data).

info(Format) ->
    info(Format, []).

info(Format, Data) ->
    log(info, Format, Data).

warn(Format) ->
    warn(Format, []).

warn(Format, Data) ->
    log(warn, Format, Data).

error(Format) ->
    error(Format, []).

error(Format, Data) ->
    log(error, Format, Data).

fatal(Format) ->
    fatal(Format, []).

fatal(Format, Data) ->
    log(fatal, Format, Data).

write(#state{level = Level} = State, MsgLevel, Pid, Format, Data) ->
    Tm = calendar:local_time(),
    case prepare_output(State, Tm) of
	{ok, NewState} ->
	    Fun = fun (Dev, Prefix) ->
			  io:fwrite(Dev, "~s ~-10s ~-16w ~s~n", 
				    [fdatetime(Tm), Prefix, Pid, 
				     io_lib:format(Format, Data)])
		  end,

	    case level_filter(Level, MsgLevel) of
		{ok, Prefix} ->
		    lists:foreach(fun (Dev) -> Fun(Dev, Prefix) end, log_fds(NewState));
		false ->
		    dummy
	    end,
	    {ok, NewState};

	{error, Reason} ->
	    {error, Reason}
    end;
write(_, _, _, _, _) ->
    ok.

fdatetime() ->
    fdatetime(calendar:local_time()).

fdatetime(Tm) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = Tm,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Min, Sec]).

-define(LVL_TRACE, 100).
-define(LVL_DEBUG, 90).
-define(LVL_INFO, 80).
-define(LVL_WARN, 70).
-define(LVL_ERROR, 60).
-define(LVL_FATAL, 50).
-define(LVL_NONE, 0).

-define(LEVEL_MAP, [
		    {trace, ?LVL_TRACE, "TRACE"},
		    {debug, ?LVL_DEBUG, "DEBUG"},
		    {info, ?LVL_INFO, "INFO"},
		    {warn, ?LVL_WARN, "WARNING"},
		    {error, ?LVL_ERROR, "ERROR"},
		    {fatal, ?LVL_FATAL, "FATAL"},
		    {none, ?LVL_NONE, ""}
		   ]).

level_filter(none, _) ->
    false;
level_filter(_, none) ->
    false;
level_filter(Current, Level) ->
    case lists:keyfind(Level, 1, ?LEVEL_MAP) of
	{Level, Rate, Prefix} ->
	    case lists:keyfind(Current, 1, ?LEVEL_MAP) of
		{Current, CurRate, _} ->
		    case CurRate >= Rate of
			true ->
			    {ok, Prefix};
			false ->
			    false
		    end;
		false ->
		    false
	    end;
	false ->
	    {ok, atom_to_list(Level)}
    end.

prepare_output(#state{logfile = undefined} = State, _) ->
    {ok, State};
prepare_output(#state{rotate = none, fd = Fd} = State, _) when Fd /= undefined ->
    {ok, State};
prepare_output(#state{logfile = LogFile, logmode = Mode, rotate = Rotate, 
	      tmRotate = TmRotate, fd = Fd} = State, Tm) 
  when (Tm >= TmRotate) or (TmRotate == undefined) ->
    NewFile = gen_log_name(Rotate, LogFile, Tm),
    NextTm = next_rotate_tm(Rotate, Tm),

    case Fd /= undefined of
	true ->
	    file:close(Fd);
	false ->
	    dummy
    end,

    case file:open(NewFile, [Mode]) of
	{ok, NewFd} ->
	    {ok, State#state{fd = NewFd, tmRotate = NextTm}};
	{error, Reason} ->
	    {error, Reason}
    end;
prepare_output(State, _) ->
    {ok, State}.

gen_log_name(none, Name, _) ->
    Name;
gen_log_name(minutely, Name, {{Y, Mo, D}, {H, Mi, _}}) ->
    io_lib:format("~s.~4..0w~2..0w~2..0w~2..0w~2..0w", [Name, Y, Mo, D, H, Mi]);
gen_log_name(hourly, Name, {{Y, Mo, D}, {H, _, _}}) ->
    io_lib:format("~s.~4..0w~2..0w~2..0w~2..0w", [Name, Y, Mo, D, H]);
gen_log_name(_, Name, {{Y, Mo, D}, _}) ->
    io_lib:format("~s.~4..0w~2..0w~2..0w", [Name, Y, Mo, D]).

next_rotate_tm(none, _) ->
    undefined;
next_rotate_tm(minutely, Tm) ->
    {Date, {H, Mi, _}} = vm_utils:datetime_add_seconds(Tm, 60),
    {Date, {H, Mi, 0}};
next_rotate_tm(hourly, Tm) ->
    {Date, {H, _, _}} = vm_utils:datetime_add_seconds(Tm, 3600),
    {Date, {H, 0, 0}};
next_rotate_tm(daily, Tm) ->
    {Date, _} = vm_utils:datetime_add_seconds(Tm, 86400),
    {Date, {0, 0, 0}};
next_rotate_tm(weekly, {Date, _}) ->
    WeekDay = calendar:day_of_the_week(Date),
    {Date1, _} = vm_utils:datetime_add_seconds({Date, {0, 0, 0}}, 86400*(7-WeekDay+1)),
    {Date1, {0, 0, 0}};
next_rotate_tm(monthly, {{Y, Mo, _}, _}) ->
    Days = calendar:last_day_of_the_month(Y, Mo),
    {{Y1, Mo1, _}, _} = vm_utils:datetime_add_seconds({{Y, Mo, 1}, {0, 0, 0}}, 86400*Days),
    {{Y1, Mo1, 1}, {0, 0, 0}};
next_rotate_tm(yearly, {{Y, _, _}, _}) ->
    {{Y+1, 1, 1}, {0, 0, 0}}.

calc_timeout(#state{tmRotate = TmRotate}) when TmRotate == undefined ->
    infinity;
calc_timeout(#state{tmRotate = TmRotate}) ->
    case vm_utils:datetime_sec_diff(TmRotate, calendar:local_time()) of
	N when N > 0 ->
	    N * 1000;
	_ ->
	    0
    end.





