
-module(vendmach_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 10001).

start(_StartType, _StartArgs) ->
    Port = ?PORT,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case vendmach_sup:start_link(LSock) of
	{ok, Pid} ->
	    vendmach_sup:start_child(),
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

stop(_State) ->
    ok.



