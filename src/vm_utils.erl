
-module(vm_utils).

-export([ifelse/3, datetime_add_seconds/2, datetime_sec_diff/2]).

ifelse(Expr, A, B) ->
    case Expr of
	true ->
	    A;
	false ->
	    B
end.

datetime_add_seconds(Date, S) ->
    Sd = calendar:datetime_to_gregorian_seconds(Date),
    calendar:gregorian_seconds_to_datetime(Sd + S).

datetime_sec_diff(Date1, Date2) ->
    S1 = calendar:datetime_to_gregorian_seconds(Date1),
    S2 = calendar:datetime_to_gregorian_seconds(Date2),
    S1 - S2.


