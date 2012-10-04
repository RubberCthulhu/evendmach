
-module(vm_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 5555).

start(_StartType, _StartArgs) ->
    Port = ?PORT,
    {ok, ListenSock} = gen_tcp(Port, [{active, true}]),
    {ok, Pid} = vm_server_sup:start_link(ListenSock),
    {ok, _} = vm_server_sup:start_child(),
    {ok, Pid}.

stop(_State) ->
    ok.




