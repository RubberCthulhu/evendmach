
-module(vm_client_sup).

-behaviour(supervisor).

% API
-export([start_link/0, start_child/0]).

% Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, vm_client_sup}, ?MODULE, []).

start_child() ->
    supervisor:start_child(vm_client_sup, []).

init([]) ->
    RestartStrategy = {simple_one_for_one, 0, 1},
    Client = {vm_client, {vm_client, start_link, []}, temporary, 
	      brutal_kill, worker, [vm_client]},
    {ok, {RestartStrategy, [Client]}}.


