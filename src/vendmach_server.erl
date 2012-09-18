
-module(vendmach_server).

-beghaviour(gen_server).

% API
-export([start_link/1]).

% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, Socket, State}) ->
    {stop, normal, state};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    vendmach_sup:start_child(),
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

handle_data(Socket, RawData, State) ->
    State.



