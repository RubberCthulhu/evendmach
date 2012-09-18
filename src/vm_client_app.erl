
-module(vm_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    vm_client_sup:start_link().

stop(_State) ->
    ok.






