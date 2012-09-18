
-module(pending).

-export([len/1, len/2, pend/1, pend/2, pend/4]).

% 64 bits
-define(BLOCK_64, 8).

len(Len) ->
    len(Len, ?BLOCK_64).

len(Len, BlockSize) ->
    case Len rem BlockSize of
	0 -> BlockSize;
	N -> BlockSize - N
    end.

pend(Data) ->
    pend(Data, ?BLOCK_64).

pend(Data, BlockSize) ->
    pend(Data, BlockSize, fun gen/1, []).

pend(Data, BlockSize, Gen, GenArgs) ->
    N = len(byte_size(Data), BlockSize),
    Pending = apply(Gen, [N | GenArgs]),
    <<Data/bytes, Pending/bytes>>.

gen(N) ->
    gen(N, N).

gen(N, Byte) ->
    binary:copy(<<Byte>>, N).








