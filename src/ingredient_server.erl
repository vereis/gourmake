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
    stop_link/0,
    populate/1,
    select/3,
    get/1,
    get/2,
    start_link/0
]).

-compile({no_auto_import, [get/1]}).

%%% Typespecs
-type ingredient() :: {atom(), ingredient_data()}.

-type ingredient_data() :: #{categories := [category()],
                             cuisines   := [cuisine()]}.

-type category() :: atom().

-type cuisine() :: atom().

-type ingredients() :: #{atom() => {string(), [category()], [cuisine()]}}.

-type cuisines() :: #{cuisine() => [atom()]}.

-type categories() :: #{category() => [atom()]}.

-type startlink_ret() :: ignore
                       | {error, _}
                       | {ok, pid()}.

-export_type([
    ingredient/0,
    category/0,
    cuisine/0,
    ingredients/0,
    cuisines/0,
    categories/0
]).

%%% Entrypoints
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop_link() -> any().
stop_link() ->
    gen_server:call(?MODULE, stop).

-spec populate(_) -> any().
populate(Ingredients) ->
    gen_server:call(?MODULE, {populate, Ingredients}).

-spec select(category() , cuisine(), [category()]) -> atom().
select(Category, Cuisine, CategoryBlacklist) ->
    gen_server:call(?MODULE, {select,
                             {Category,
                              Cuisine,
                              CategoryBlacklist}}).

-spec get(categories)  -> categories();
         (cuisines)    -> cuisines();
         (ingredients) -> ingredients().
get(categories) -> 
    gen_server:call(?MODULE, {get, categories});
get(cuisines) ->
    gen_server:call(?MODULE, {get, cuisines});
get(ingredients) ->
    gen_server:call(?MODULE, {get, ingredients}).

-spec get(categories, category()) -> [atom()];
         (cuisines, cuisine()) -> [atom()];
         (ingredients, atom()) -> {string(), [category()], [cuisine()]}
                                | {err, bad_ingredient, atom()}.
get(categories, Category) -> 
    gen_server:call(?MODULE, {get, category, Category});
get(cuisines, Cuisine) ->
    gen_server:call(?MODULE, {get, cuisine, Cuisine});
get(ingredients, Ingredient) ->
    gen_server:call(?MODULE, {get, ingredient, Ingredient}).

%%% Gen_server call handler
handle_call({populate, Ingredients}, _From, _DB) ->
    {reply, {populated, Ingredients}, Ingredients};

handle_call({get, ingredients}, _From, DB) ->
    {I, _, _} = DB,
    {reply, I, DB};

handle_call({get, ingredient, Ingredient}, _From, DB) ->
    {I, _, _} = DB,
    case maps:is_key(Ingredient, I) of
        true -> {reply, maps:get(Ingredient, I), DB};
        _    -> {reply, {err, bad_ingredient, Ingredient}, DB}
    end;

handle_call({get, categories}, _From, DB) ->
    {_, C, _} = DB,
    {reply, C, DB};

handle_call({get, category, Category}, _From, DB) ->
    {_, C, _} = DB,
    case maps:is_key(Category, C) of
        true -> {reply, maps:get(Category, C), DB};
        _    -> {reply, {err, bad_category, Category}, DB}
    end;

handle_call({get, cuisines}, _From, DB) ->
    {_, _, C} = DB,
    {reply, C, DB};

handle_call({get, cuisine, Cuisine}, _From, DB) ->
    {_, _, C} = DB,
    case maps:is_key(Cuisine, C) of
        true -> {reply, maps:get(Cuisine, C), DB};
        _    -> {reply, {err, bad_cuisine, Cuisine}, DB}
    end;

handle_call(stop, _From, DB) ->
    {stop, normal, shutdown_ok, DB}.

%%% Gen_server boilerplate
-spec init([]) -> {ok, {ingredients(), categories(), cuisines()}}.
init([]) ->
    {ok, [Raw]} = file:consult(<<"../data/ingredients.term">>),
    DB = process_data(Raw),
    {ok, DB}.

-spec handle_cast(_, _) -> {noreply, _}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, _) -> {noreply, _}.
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, _, _) -> {ok, _}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%% Private Functions

%% Fold over ingredient data and build three maps out of it allowing quick access
%% to ingredients themselves, categories containing ingredients and cuisines containing
%% ingredients
-spec process_data([ingredient()]) -> {ingredients(), categories(), cuisines()}.
process_data(IngredientData) ->
    lists:foldl(fun({I, Props}, {Ingredients, Categories, Cuisines}) ->
        % Build updated Ingredients
        UIngredients = maps:put(I, {util:pretty_print_atom(I), 
                                    maps:get(categories, Props), 
                                    maps:get(cuisines, Props)},
                                Ingredients),
        
        % Build updated Categories
        UCategories = lists:foldl(fun(Category, Categories) ->
            maps:update_with(Category, fun(V) -> [I | V] end, [I], Categories)
        end, Categories, maps:get(categories, Props)),
        
        % Build updated Cuisines
        UCuisines = lists:foldl(fun(Cuisine, Cuisines) ->
            maps:update_with(Cuisine, fun(V) -> [I | V] end, [I], Cuisines)
        end, Cuisines, maps:get(cuisines, Props)),

        % Return new accumulator
        {UIngredients, UCategories, UCuisines}
end, {#{}, #{}, #{}}, IngredientData).
