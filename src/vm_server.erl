
-module(vm_server).

-behaviour(gen_server).

% API
-export([start/0, start/1, stop/0, start_link/1]).

% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {lsock}).

start() ->
    application:start(vm_server).

start(Type) ->
    application:start(vm_server, Type).

stop() ->
    application:stop(vm_server).

start_link(ListenSock) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, #state{lsock = ListenSock}, 0}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Socket, _RawData}, State) ->
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = ListenSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(ListenSock),
    vm_server_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






