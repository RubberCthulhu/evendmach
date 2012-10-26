
-module(vm_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 5555).
-define(LOG_FILE, "/home/alevandal/work/projects/evendmach/var/vendmach.log").

start(_StartType, _StartArgs) ->
    Port = ?PORT,
    {ok, _} = vm_logger_sup:start_link([{logfile, ?LOG_FILE}, {level, trace},
					{rotate, daily}]),
    vm_logger:info("Starting..."),
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]),
    {ok, Pid} = vm_server_sup:start_link(ListenSock),
    {ok, _} = vm_server_sup:start_child(),
    vm_logger:info("Started successfully"),
    {ok, Pid}.

stop(_State) ->
    ok.




