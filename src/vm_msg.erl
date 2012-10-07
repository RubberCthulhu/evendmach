
-module(vm_msg).

-export([encode/3, decode/3, decode_req/3]).

-define(MAX_SIZE, 1024).

-define(CMD_START, 1).
-define(CMD_BUY, 2).
-define(CMD_ACCEPT, 3).
-define(CMD_CANCEL, 4).
-define(CMD_BALANCE, 5).
-define(CMD_PUT, 8).

-define(CLIENT_ID_MAX_SIZE, 50).
-define(CARD_NUMBER_MAX_SIZE, 40).

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

decode_req(Key, IVec, Data) ->
    case decode(Key, IVec, Data) of
	{ok, RawMsg, Rest} ->
	    ParseInfo = parse_req(RawMsg),
	    {ok, ParseInfo, Rest};
	{error, Reason} ->
	    {error, Reason}
    end.

parse_req(Data) when is_binary(Data) ->
    parse_req(binary:bin_to_list(Data));
parse_req(Data) when is_list(Data) ->
    case lists:map(fun string:strip/1, string:tokens(Data, ";")) of
	[Cmd | Args] ->
	    parse_req(Cmd, Args);
	_ ->
	    {error, command_unspecified}
    end.

%check_cmd([Cmd | Rest]) ->
%    case string:to_integer(Cmd) of
%	{Code, []} when check_cmd_code(Code) ->
%	    check_client(Code, Rest);
%	{Code, []} ->
%	    {unknown, Code, Rest};
%	_ ->
%	    {error, invalid_format}
%    end.

parse_req(Cmd, Args) when is_list(Cmd) ->
    case string:to_integer(Cmd) of
	{CmdCode, []} ->
	    parse_req(CmdCode, Args);
	_ ->
	    {error, invalid_format}
    end;
%parse_req(?CMD_BALANCE, [Client, CardNumber, CardType]) ->
%    {balance, Client, CardNumber, CardType};
parse_req(Cmd, Args) ->
    {unknown, Cmd, Args}.

parse_client(Client) when length(Client) =< ?CLIENT_ID_MAX_SIZE ->
    {ok, Client};
parse_client(_) ->
    {error}.

parse_int(S) ->
    case string:to_integer(S) of
	{N, []} ->
	    {ok, N};
	_ ->
	    {error}
    end.

is_hex_digit(C) ->
    (C => $0 and C =< 9) or (C => $a and C =< $f) or (C => $A and C =< $F).

parse_card_number(Number) when length(Number) =< ?CARD_NUMBER_MAX_SIZE ->
    case lists:all(fun is_hex_digit/1, Number) of
	true ->
	    {ok, Number};
	false ->
	    {error}
    end;
parse_card_number(_) ->
    {error}.

