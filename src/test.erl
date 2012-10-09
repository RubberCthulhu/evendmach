
-module(test).

-export([parse/1]).

parse(Data) when is_binary(Data) ->
    parse(binary:bin_to_list(Data));
parse(Data) when is_list(Data) ->
    List = lists:map(fun string:strip/1, string:tokens(Data, ";")),
    parse_msg(List).

parse_msg([]) ->
    {error, command_unspecified};
parse_msg([First | Rest]) ->
    case string:to_integer(First) of
	{Cmd, []} ->
	    parse_msg(Cmd, Rest);
	_ ->
	    {error, invalid_format, First, Rest}
    end.

parse_msg(Cmd, Args) ->
    parse_cmd(Cmd, Args).

parse_cmd(5, Args) ->
    Parsers = [fun client_id/1, fun card_number/1, fun card_type/1],
    case parse_args(Parsers, Args) of
	{ok, [Client, CardNumber, CardType], _} ->
	    {ok, 5, Client, CardNumber, CardType};
	{error, Reason} ->
	    {error, Reason, 5, Args}
    end;
parse_cmd(Cmd, Args) ->
    {error, unknown_command, Cmd, Args}.

parse_args(Parsers, Args) ->
    parse_args(Parsers, Args, []).

parse_args([], Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_args(_, [], _) ->
    {error, parameters_absent};
parse_args([Parser | Parsers], [Arg | Rest], Acc) ->
    case Parser(Arg) of
	{ok, Value} ->
	    parse_args(Parsers, Rest, [Value | Acc]);
	error ->
	    {error, invalid_format}
    end.

client_id(Client) when length(Client) =< 50 ->
    {ok, Client};
client_id(_) ->
    error.

card_number(Number) when length(Number) =< 40 ->
    {ok, Number};
card_number(_) ->
    error.

card_type(Type) ->
    case string:to_integer(Type) of
	{T, []} ->
	    {ok, T};
	_ ->
	    error
    end.





