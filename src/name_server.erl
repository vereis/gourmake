-module(name_server).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    stop_link/0,
    get/0,
    start_link/0
]).

-type startlink_ret() :: ignore
                       | {error, _}
                       | {ok, pid()}.

-type handlecall_ret() :: {reply, _, _}
                        | {stop, normal, shutdown_ok, _}.

%%% Entrypoints
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop_link() -> any().
stop_link() ->
    gen_server:call(?MODULE, stop).

-spec get() -> string().
get() ->
    gen_server:call(?MODULE, get).


%%% Gen_server call handler
-spec handle_call(get | stop, _, _) -> handlecall_ret().
handle_call(get, _From, DB) ->
    {reply, util:pick_random(DB), DB};

handle_call(stop, _From, DB) ->
    {stop, normal, shutdown_ok, DB}.


%%% Gen_server boilerplate
-spec init([]) -> {ok, [string(), ...]}.
init([]) ->
    {ok, [DB]} = file:consult(<<"../data/names.term">>),
    {ok, DB}.

-spec handle_cast(_ ,_) -> {'noreply', _}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, _) -> {'noreply', _}.
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(_, _) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, _, _) -> {'ok', _}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
