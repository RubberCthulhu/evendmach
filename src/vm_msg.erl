
-module(vm_msg).

-export([encode/3, decode/3]).

-define(MAX_SIZE, 1024).

encode(Key, IVec, Msg) ->
    Data = padding:pad(Msg),
    DesData = crypto:des_cbc_encrypt(Key, IVec, Data),
    Len = byte_size(DesData),
    <<Len:32, DesData/bytes>>.

decode(_, _, <<Len:32, _/bytes>>) when Len > ?MAX_SIZE ->
    {error, max_size};
decode(Key, IVec, <<Len:32, EncMsg:Len/bytes, Rest/bytes>>) ->
    try
	DecMsg = crypto:des_cbc_decrypt(Key, IVec, EncMsg),
	Msg = padding:unpad(DecMsg),
	{ok, Msg, Rest}
    catch
	throw:Reason ->
	    {error, Reason};
	error:Reason ->
	    {error, Reason}
    end;
decode(_, _, _) ->
    {error, data_lack}.






