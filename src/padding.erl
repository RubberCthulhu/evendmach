
-module(padding).

-export([len/1, len/2, pad/1, pad/2, pad/4, unpad/1]).

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

unpad(Data) when is_binary(Data) ->
    binary:part(Data, 0, byte_size(Data)-binary:last(Data)).

gen(N) ->
    gen(N, N).

gen(N, Byte) ->
    binary:copy(<<Byte>>, N).








