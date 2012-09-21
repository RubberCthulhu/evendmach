
-module(vm_msg).

-export([encode/3]).

encode(Key, IVec, Msg) ->
    Data = padding:pad(Msg),
    DesData = crypto:des_cbc_encrypt(Key, IVec, Data),
    Len = byte_size(DesData),
    <<Len:32, DesData/bytes>>.

%decode_len(<<Len:32, Payload/bytes>>) ->
%    {ok, Len};
%decode_len(_) ->
%    {error, size_lack}.







