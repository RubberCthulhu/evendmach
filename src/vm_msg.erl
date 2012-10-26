
-module(vm_msg).

-export([encode/3, decode/3, decode_req/3, parse/1]).

-define(MAX_SIZE, 1024).

%-define(CMD_START, 1).
%-define(CMD_BUY, 2).
%-define(CMD_ACCEPT, 3).
%-define(CMD_CANCEL, 4).
%-define(CMD_BALANCE, 5).
%-define(CMD_PUT, 8).

-define(CLIENT_ID_MAX_SIZE, 50).
-define(CARD_NUMBER_MAX_SIZE, 40).
-define(PRODUCT_CODE_MAX_SIZE, 19).

%-record(vmReq, {cmd, clientId, cardNumber, cardType, tId, product, money}).
-include("vm_msg.hrl").

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
	    ParseInfo = parse(RawMsg),
	    {ok, ParseInfo, Rest};
	{error, Reason} ->
	    {error, Reason}
    end.

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
	    parse_cmd(Cmd, Rest);
	_ ->
	    {error, invalid_format, First, Rest}
    end.

parse_cmd(Cmd, Args) when Cmd == ?CMD_START ->
    parse_cmd(Cmd, [fun client_id/1, fun card_number/1, fun int/1], Args, 
	fun ([C, N, T]) ->
		#vmReq{cmd = Cmd, clientId = C, cardNumber = N, cardType = T}
	end);
parse_cmd(Cmd, Args) when Cmd == ?CMD_BUY ->
    parse_cmd(Cmd, [fun client_id/1, fun int/1, fun product/1, fun int/1], Args, 
	fun ([C, T, P, M]) ->
		#vmReq{cmd = Cmd, clientId = C, tId = T, product = P, money = M}
	end);
parse_cmd(Cmd, Args) when Cmd == ?CMD_ACCEPT ->
    parse_cmd(Cmd, [fun client_id/1, fun int/1], Args, 
	fun ([C, T]) ->
		#vmReq{cmd = Cmd, clientId = C, tId = T}
	end);
parse_cmd(Cmd, Args) when Cmd == ?CMD_CANCEL ->
    parse_cmd(Cmd, [fun client_id/1, fun int/1], Args, 
	fun ([C, T]) ->
		#vmReq{cmd = Cmd, clientId = C, tId = T}
	end);
parse_cmd(Cmd, Args) when Cmd == ?CMD_BALANCE ->
    parse_cmd(Cmd, [fun client_id/1, fun card_number/1, fun int/1], Args, 
	fun ([C, N, T]) ->
		#vmReq{cmd = Cmd, clientId = C, cardNumber = N, cardType = T}
	end);
parse_cmd(Cmd, Args) when Cmd == ?CMD_PUT ->
    parse_cmd(Cmd, [fun client_id/1, fun int/1, fun int/1], Args, 
	fun ([C, N, T]) ->
		#vmReq{cmd = Cmd, clientId = C, cardNumber = N, cardType = T}
	end);
parse_cmd(Cmd, Args) ->
    {error, unknown_command, Cmd, Args}.

parse_cmd(Cmd, Parsers, Args, Prod) ->
    case parse_args(Parsers, Args) of
	{ok, Values, _} ->
	    Prod(Values);
	{error, Reason} ->
	    {error, Reason, Cmd, Args}
    end.

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

client_id(Client) when length(Client) =< ?CLIENT_ID_MAX_SIZE ->
    {ok, Client};
client_id(_) ->
    error.

card_number(Number) when length(Number) =< ?CARD_NUMBER_MAX_SIZE ->
    Number1 = string:to_upper(Number),
    case lists:all(fun (C) -> 
			   ((C >= $0) and (C =< $9)) or ((C >= $A) and (C =< $F)) end, 
		   Number1) of
	true ->
	    {ok, Number1};
	false ->
	    error
    end;
card_number(_) ->
    error.

product(Product) when length(Product) =< ?PRODUCT_CODE_MAX_SIZE ->
    {ok, Product};
product(_) ->
    error.

int(S) ->
    case string:to_integer(S) of
	{X, []} ->
	    {ok, X};
	_ ->
	    error
    end.




