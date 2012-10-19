
-module(vm_logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Callback
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, vm_logger_sup}, ?MODULE, Args).

init(Args) ->
    RestartStrategy = {one_for_one, 0, 1},
    Logger = {vm_logger, {vm_logger, start_link, [Args]}, permanent,
	      brutal_kill, worker, [vm_logger]},
    {ok, {RestartStrategy, [Logger]}}.






