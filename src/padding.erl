
-module(padding).

-export([len/1, len/2, pad/1, pad/2, unpad/1, unpad/2]).

% 64 bits
-define(BLOCK_64, 8).

len(Len) ->
    len(Len, ?BLOCK_64).

len(Len, BlockSize) ->
    case Len rem BlockSize of
	0 -> BlockSize;
	N -> BlockSize - N
    end.

pad(Data) ->
    pad(Data, ?BLOCK_64).

pad(Data, BlockSize) ->
    pad(Data, BlockSize, fun gen/1, []).

pad(Data, BlockSize, Gen, GenArgs) when is_list(Data) ->
    pad(binary:list_to_bin(Data), BlockSize, Gen, GenArgs);
pad(Data, BlockSize, Gen, GenArgs) when is_binary(Data) ->
    N = len(byte_size(Data), BlockSize),
    Padding = apply(Gen, [N | GenArgs]),
    <<Data/bytes, Padding/bytes>>.

unpad(Data) ->
    unpad(Data, ?BLOCK_64).

unpad(Data, BlockSize) when is_binary(Data) ->
    Len = byte_size(Data),
    Last = binary:last(Data),
    if
	(Len rem BlockSize) == 0, Last =< Len, Last =< BlockSize ->
	    ActualLen = Len - Last,
	    Padding = binary:copy(<<Last>>, Last),
	    case Data of
		<<Actual:ActualLen/bytes, Padding/bytes>> -> Actual;
		_ -> throw(incorrect_padding)
	    end;
	true -> throw(incorrect_data)
    end.

gen(N) ->
    gen(N, N).

gen(N, Byte) ->
    binary:copy(<<Byte>>, N).








