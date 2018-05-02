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


%%% Entrypoints
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_link() ->
    gen_server:call(?MODULE, stop).

get() ->
    gen_server:call(?MODULE, get).


%%% Gen_server call handler
handle_call(get, _From, DB) ->
    {reply, util:pick_random(DB), DB};

handle_call(stop, _From, DB) ->
    {stop, normal, shutdown_ok, DB}.


%%% Gen_server boilerplate
init([]) ->
    {ok, [DB]} = file:consult(<<"../data/names.term">>),
    {ok, DB}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
