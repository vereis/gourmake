-module(ingredient_server).
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
    populate/1,
    select/3,
    start_link/0
]).

%%% Entrypoints
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

populate(Ingredients) ->
    gen_server:call(?MODULE, {populate, Ingredients}).

select(CategoryWhitelist, CategoryBlacklist, CuisineWhitelist) ->
    gen_server:call(?MODULE, {select,
                             {CategoryWhitelist,
                              CategoryBlacklist,
                              CuisineWhitelist}}).

%%% Gen_server boilerplate
init([]) ->
    DB = #{},
    {ok, DB}.

handle_call({populate, Ingredients}, _From, _DB) ->
    {reply, {populated, Ingredients}, Ingredients};

handle_call({select, {CatWhitelist, CatBlacklist, CuiWhitelist}}, _From, DB) ->
    {reply, DB, DB}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
