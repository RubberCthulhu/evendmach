
-module(bin_to_hex).

-export([bin_to_hex/1, bin_to_hex_list/1]).

-define(FILLER, " ").

bin_to_hex(Bin) when is_binary(Bin) ->
    List = bin_to_hex_list(Bin),
    string:join(List, ?FILLER).

bin_to_hex_list(Bin) when is_binary(Bin) ->
    List = binary:bin_to_list(Bin),
    lists:map(fun byte_to_hex/1, List).

byte_to_hex(Byte) ->
    S = integer_to_list(Byte, 16),
    case length(S) of
	1 ->
	    "0" ++ S;
	_ -> S
    end.


