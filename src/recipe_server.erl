-module(recipe_server).
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
    generate/0,
    get/0,
    start_link/0
]).

-define(USE_CUISINE_WHITELIST_PROB, 5).

%%% Entrypoints
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_link() ->
    gen_server:call(?MODULE, stop).

get() ->
    gen_server:call(?MODULE, get).

generate() ->
    gen_server:call(?MODULE, generate).


%%% Gen_server call handler
handle_call(get, _From, DB) ->
    {reply, DB, DB};

handle_call(generate, _From, DB) ->
    Recipe = util:pick_random(DB),
    Reply = generate_recipe(Recipe),
    {reply, Reply, DB};

handle_call(stop, _From, DB) ->
    {stop, normal, shutdown_ok, DB}.


%%% Gen_server boilerplate
init([]) ->
    {ok, [DB]} = file:consult(<<"../data/recipes.term">>),
    {ok, DB}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%% Private functions

%% Probabalistically generates a random recipe based on an existing recipe template
generate_recipe(Recipe) ->
    Cuisine = util:pick_random(maps:get(cuisines, Recipe)),
    Ingredients = maps:map(fun(_, {Min, Max, Categories, CategoryBlacklist}) ->
        Quantity = util:pick_random(lists:seq(Min, Max)),
        Is = [select_ingredient(util:pick_random(Categories), Cuisine, CategoryBlacklist) || _ <- lists:seq(1, Quantity)],
        lists:usort(Is)
    end, maps:get(ingredients, Recipe)),
    Instructions = [interpolate_recipe_string(Instr, Ingredients) || Instr <- maps:get(steps, Recipe)],

    % Now, we want to generate printable strings containing all of the information we've generated
    % before returning
    RecipeContents = {pretty_print_ingredients(Ingredients), 
                      pretty_print_instructions(Instructions)},
    
    {name_recipe(Recipe, Cuisine, Ingredients), RecipeContents}.

%% Names a recipe based on the recipe template, cuisine and ingredients
name_recipe(Recipe, Cuisine, Ingredients) ->
    {RecipeName, RecipeLiterals} = interpolate_recipe_string(maps:get(name, Recipe), Ingredients),
    CuisinePrefix = case Cuisine of
        all -> "";
        _   -> [util:pretty_print_atom(Cuisine) | [" inspired "]]
    end,
    NamePrefix = name_server:get(),
    [["# ", NamePrefix, CuisinePrefix, io_lib:format(RecipeName, RecipeLiterals)]].

%% Selects ingredients from the ingredient server of a certain category/cuisine
select_ingredient(Category, Cuisine, CategoryBlacklist) ->
    Ingredients = ingredient_server:get(categories, Category),
    IngredientBlacklist  = lists:usort(lists:flatten([ingredient_server:get(categories, C) || 
                                                      C <- CategoryBlacklist])),
    
    ProcessedIngredients = [I || I <- Ingredients, not lists:member(I, IngredientBlacklist)],
    

    % We only want to /probabalistically/ apply the cuisine whitelist
    AvailableIngredients = case rand:uniform(100) > ?USE_CUISINE_WHITELIST_PROB of
        true -> CuisineIngredients = lists:usort(ingredient_server:get(cuisines, Cuisine) ++ 
                                                 ingredient_server:get(cuisines, all)),
                [I || I <- ProcessedIngredients, lists:member(I, CuisineIngredients)];
        _    -> ProcessedIngredients
    end,
   
    % We also want to make sure AvailableIngredients has at least one element in it
    case length(AvailableIngredients) > 0 of
        true -> util:pick_random(AvailableIngredients);
        _    -> util:pick_random(ProcessedIngredients)
    end.

%% Takes a recipe instruction and interpolates values in place of placeholders in the instruction
interpolate_recipe_string(Instr, Ingredients) ->
    {ok, Regex} = re:compile("\~([a-z_-]+)"),
    case re:run(Instr, Regex, [global]) of
        nomatch -> {Instr, []};
        {match, Matches} -> 
            ProcessedInstr = binary_to_list(iolist_to_binary([re:replace(Instr, Regex, "~s", [global])])),
            Flags = [list_to_existing_atom(string:slice(Instr, M1, M2)) || [_, {M1, M2}] <- Matches],
            ProcessedFlags = [util:grammatical_concatenate(maps:get(F, Ingredients)) || F <- Flags],
            {ProcessedInstr, ProcessedFlags}
    end.

%% Pretty printing functions
pretty_print_ingredients(Ingredients) ->
    IList  = maps:to_list(Ingredients),
    Output = lists:foldl(fun({_, Is}, Acc) ->
        Acc ++ [["- " | util:pretty_print_atom(I) ] || I <- Is]
    end, [], IList),
    ["## Ingredients:" | Output].

pretty_print_instructions(Instructions) ->
    Output = lists:map(fun(I) ->
        {TemplateString, TemplateLiterals} = lists:nth(I, Instructions),
        [integer_to_list(I), ") ", io_lib:format(TemplateString, TemplateLiterals)]
    end, lists:seq(1, length(Instructions))),
    ["## Instructions:" | Output].
    
