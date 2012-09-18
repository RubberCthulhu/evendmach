
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
    case Len rem BlockSize of
	0 -> ok;
	_ -> throw(incorrect_size)
    end,
    case (Last > Len) or (Last > BlockSize) of
	true -> throw(incorrect_padding);
	_ -> ok
    end,
    binary:part(Data, 0, Len-Last).

gen(N) ->
    gen(N, N).

gen(N, Byte) ->
    binary:copy(<<Byte>>, N).








