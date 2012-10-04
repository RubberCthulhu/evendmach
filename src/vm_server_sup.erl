
-module(vm_server_sup).

-behaviour(supervisor).

% API
-export([start_link/1, start_child/0]).

% Callbacks
-export([init/1]).

start_link(ListenSock) ->
    supervisor:start_link({local, vm_server_sup}, ?MODULE, [ListenSock]).

start_child() ->
    supervisor:start_child(vm_server_sup, []).

init([ListenSock]) ->
    RestartStrategy = {sumple_one_for_one, 0, 1},
    Child = {vm_server, {vm_server, start_link, [ListenSock]}, temporary,
	     brutal_kill, worker, [vm_server]},
    {ok, {RestartStrategy, [Child]}}.



