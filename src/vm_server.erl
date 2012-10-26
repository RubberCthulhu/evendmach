
-module(vm_server).

-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/0, start_link/1]).

%% Callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(KEY, <<16#01, 16#ae, 16#32, 16#ff, 16#d9, 16#13, 16#41, 16#fb>>).
-define(IVEC, <<16#ab, 16#00, 16#33, 16#49, 16#91, 16#88, 16#ab, 16#cd>>).

-record(state, {sock, conn_state, buffer}).

-include("vm_msg.hrl").

start() ->
    application:start(vm_server).

start(Type) ->
    application:start(vm_server, Type).

stop() ->
    application:stop(vm_server).

start_link(ListenSock) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, #state{sock = ListenSock, conn_state = listen, buffer = <<>>}, 0}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Sock, RawData}, #state{buffer = Buffer} = State) ->
    Len = byte_size(RawData),
    Hex = bin_to_hex:bin_to_hex(RawData),
    vm_logger:debug("vm_server: Data received: ~w: ~s", [Len, Hex]),
    case process(<<Buffer/bytes, RawData/bytes>>, fun on_msg_received/2, []) of
	{ok, Rest, _} ->
	    {noreply, State#state{buffer = Rest}};
	{error, Reason, _, _} ->
	    vm_logger:error("vm_server: Process data error: ~p", [Reason]),
	    vm_logger:info("vm_server: Close connection"),
	    {stop, normal, State}
    end;
handle_info({tcp_closed, _Sock}, State) ->
    vm_logger:info("vm_server: Connection closed by peer"),
    {stop, normal, State};
handle_info(timeout, #state{sock = ListenSock} = State) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    vm_logger:info("vm_server: New connection"),
    vm_server_sup:start_child(),
    {noreply, State#state{sock = Sock, conn_state = handle}}.

on_msg_received(Msg, UserData) ->
    vm_logger:info("vm_server: Message received: ~p", [Msg]),
    process_request(Msg, UserData).
    %{ok, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process(<<>>, _, UserData) ->
    {ok, <<>>, UserData};
process(Data, Callback, UserData) ->
    %case vm_msg:decode(?KEY, ?IVEC, Data) of
    case vm_msg:decode_req(?KEY, ?IVEC, Data) of
	{ok, Msg, Rest} ->
	    %NewUserData = Callback(Msg, UserData),
	    case Callback(Msg, UserData) of
		{ok, NewUserData} ->
		    process(Rest, Callback, NewUserData);
		{error, Reason, NewUserData} ->
		    {error, Reason, Data, NewUserData}
	    end;
	{error, data_lack} ->
	    {ok, Data, UserData};
	{error, Reason} ->
	    {error, Reason, Data, UserData}
    end.

process_request({error, Reason}, UserData) ->
    vm_logger:error("vm_server: Request error: ~p: close connection", [Reason]),
    {error, Reason, UserData};
process_request({error, Reason, _Cmd, _}, _UserData) ->
    vm_logger:error("vm_server: Request error: ~p: send error responce", [Reason]),
    {ok, _UserData};
process_request(#vmReq{cmd = Cmd} = _Req, UserData) ->
    vm_logger:info("vm_server: request received: ~w", [Cmd]),
    {ok, UserData}.

%send_reply() ->
%    ok.







