
-module(vm_store).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% Store API
-export([
	 get_card_info/2,
	 balance_update/2,
	 balance_inc/2,
	 balance_dec/2,
	 action_buy/3,
	 action_cancel/2,
	 action_replanish/2
]).

%% Callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dbCon}).

start(ConnStr) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [ConnStr], []).

start_link(ConnStr) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConnStr], []).

init([ConnStr]) ->
    ConnOpts = [{scrollable_cursors, off}, {auto_commit, off}],
    case odbc:connect(ConnStr, ConnOpts) of
	{ok, Con} ->
	    {ok, #state{dbCon = Con}};
	Error ->
	    {stop, Error}
    end.

terminate(_Reason, #state{dbCon = Con}) ->
    odbc:disconnect(Con),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({card_info, Number, Type}, _From, #state{dbCon = Con} = State) ->
    Query = "SELECT id, card_number, card_type, card_owner, card_status, card_balance "
	"FROM vm_card WHERE card_number = ? AND card_type = ?",
    Params = [{{sql_varchar, 32}, [Number]}, {sql_integer, Type}],
    case odbc:param_query(Con, Query, Params) of
	{selected, _, [Record | []]} ->
	    {reply, {ok, prepare_card_info(Record)}, State};
	{selected, _, []} ->
	    {reply, {error, not_found}, State};
	{selected, _, [_ | _]} ->
	    {reply, {error, multiple_definition}, State};
	Error ->
	    {reply, Error, State}
    end;
handle_call({balance_update, Id, N}, _From, #state{dbCon = Con} = State) ->
    Query = "UPDATE vm_card SET card_balance = card_balance + ? WHERE id = ?",
    Params = [{sql_integer, [N]}, {sql_integer, [Id]}],
    case odbc:param_query(Con, Query, Params) of
	{updated, _} ->
	    case odbc:commit(Con, commit) of
		ok ->
		    {reply, ok, State};
		Error ->
		    {reply, Error, State}
	    end;
	Error ->
	    odbc:commit(Con, rollback),
	    {reply, Error, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%transaction([], Acc, #state{dbCon = Con} = State) ->
%    case odbc:commit(Con, commit) of
%	ok	

prepare_card_info({Id, Number, Type, Owner, Status, Balance}) ->
    {trunc(Id), Number, trunc(Type), Owner, trunc(Status), trunc(Balance)}.

get_card_info(Number, Type) ->
    gen_server:call(?SERVER, {card_info, Number, Type}).

balance_update(Id, N) ->
    gen_server:call(?SERVER, {balance_update, Id, N}).

balance_inc(Id, N) ->
    balance_update(Id, N).

balance_dec(Id, N) ->
    balance_update(Id, N*(-1)).

action_buy(Id, Amount, Product) ->
    gen_server:call(?SERVER, {action_buy, Id, Amount, Product}).

action_cancel(Id, Amount) ->
    gen_server:call(?SERVER, {action_cancel, Id, Amount}).

action_replanish(Number, Amount) ->
    gen_server:call(?SERVER, {action_replanish, Number, Amount}).



