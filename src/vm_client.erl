
-module(vm_client).

-behaviour(gen_server).

% API
-export([start/0, start/1, stop/0,
	start_link/0]).

% Callbacks
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([connect/2, disconnect/1, send/2]).

-record(state, {sock, conn_state}).

start() ->
    application:start(vm_client).

start(Type) ->
    application:start(vm_client, Type).

stop() ->
    application:stop(vm_client).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{sock = 0, conn_state = closed}, infinity}.

handle_call({connect, Host, Port}, _From, State) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
	{ok, Sock} ->
	    NewState = #state{sock = Sock, conn_state = connected},
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({disconnect}, _From, #state{sock = Sock, conn_state = ConnState} = _State)
  when ConnState == connected ->
    Result = gen_tcp:close(Sock),
    NewState = #state{sock = Sock, conn_state = closed},
    {reply, Result, NewState};
handle_call({send, Payload}, _From, #state{sock = Sock, conn_state = ConnState} = State)
  when ConnState == connected ->
    Result = gen_tcp:send(Sock, Payload),
    {reply, Result, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.
%handle_call(Msg, _From, State) ->
%    {reply, {1, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Sock, _RawData}, State) ->
    {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock = Sock, conn_state = ConnState} = _State) ->
    case ConnState of
	connected ->
	    gen_tcp:close(Sock);
	closed ->
	    ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(Host, Port) ->
    {ok, Pid} = vm_client_sup:start_child(),
    connect(Pid, Host, Port).

connect(ConnRef, Host, Port) ->
    case call(ConnRef, {connect, Host, Port}) of
	ok ->
	    {ok, ConnRef};
	Error ->
	    Error
    end.

disconnect(ConnRef) ->
    Result = call(ConnRef, {disconnect}),
    cast(ConnRef, stop),
    Result.

send(ConnRef, Msg) when is_list(Msg) ->
    send(ConnRef, binary:list_to_bin(Msg));
send(ConnRef, Msg) when is_binary(Msg) ->
    call(ConnRef, {send, Msg}).

call(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).



