-module(main).
-export([
    main/0,
    select_ingredient/3
]).

-define(USE_CUISINE_WHITELIST_PROB, 5).
-define(MUTATIONPROB, 5).

main() ->
    {Data, Recipes, Names} = bootstrap:bootstrap(),
    {RecipeName, RecipeIngredients, RecipeInstructions} = parse_recipe(recipe_server:select(), Data),
    util:pretty_print_list(RecipeName),
    util:pretty_print_list(RecipeIngredients),
    util:pretty_print_list(RecipeInstructions).

parse_recipe(Recipe, Data) ->
    #{cuisines := AvailableCuisines} = Recipe,
    ChosenCuisine = util:pick_random(AvailableCuisines),

    Ingredients = parse_recipe_ingredients(Recipe, ChosenCuisine, Data),
    Steps = parse_recipe_steps(Recipe, Ingredients),
    
    format_output(ChosenCuisine, Recipe, Ingredients, Steps).

parse_recipe_ingredients(Recipe, Cuisine, Data) ->
    #{ingredients := I} = Recipe,
    {_, Categories, Cuisines} = Data,

    maps:map(fun(_, {MinQ, MaxQ, Category, FilterCategories}) ->
        Q = util:pick_random(lists:seq(MinQ, MaxQ)),
        RawData = [select_ingredient(util:pick_random(Category), Cuisine, FilterCategories) 
                   || _ <- lists:seq(1, Q)],
        lists:usort(RawData)
    end, I).

parse_recipe_name(Recipe, Ingredients) ->
    #{name := N} = Recipe,
    interpolate_recipe_string(N, Ingredients).

parse_recipe_steps(Recipe, Ingredients) ->
    #{steps := S} = Recipe,
    [interpolate_recipe_string(Step, Ingredients) || Step <- S]. 

interpolate_recipe_string(Str, Ingredients) ->
    {ok, Regex} = re:compile("\~([a-z_-]+)"),
    case re:run(Str, Regex, [global]) of
        nomatch -> {Str, []};
        {match, Matches} -> 
            ProcessedStr = binary_to_list(iolist_to_binary([re:replace(Str, Regex, "~s", [global])])),
            Flags = [list_to_existing_atom(string:slice(Str, M1, M2)) || [_, {M1, M2}] <- Matches],
            ProcessedFlags = [util:grammatical_concatenate(maps:get(F, Ingredients)) || F <- Flags],
            {ProcessedStr, ProcessedFlags}
    end.

format_ingredients(Ingredients) ->
    IngredientsList = maps:to_list(Ingredients),
    IngredientStrings = lists:foldl(fun({_, Is}, Acc) ->
        Acc ++ lists:map(fun(I) ->
            ["- ", util:pretty_print_atom(I)]
        end, Is)
    end, [], IngredientsList),
    ["## Ingredients:" | IngredientStrings].

format_instructions(Instructions) ->
    InstructionStrings = lists:map(fun(I) ->
        {TemplateString, TemplateLiterals} = lists:nth(I, Instructions),
        [integer_to_list(I), ") ", io_lib:format(TemplateString, TemplateLiterals)]
    end, lists:seq(1, length(Instructions))),
    ["## Instructions:" | InstructionStrings].

format_recipe_name(Cuisine, Recipe, Ingredients) ->
    {RecipeName, RecipeLiterals} = parse_recipe_name(Recipe, Ingredients),
    CuisineString = case Cuisine of
        all -> "";
        _   -> [string:lowercase(util:pretty_print_atom(Cuisine)) | ["-inspired "]]
    end,

    [["# ", CuisineString, io_lib:format(RecipeName, RecipeLiterals)]].

format_output(Cuisine, Recipe, Ingredients, Instructions) ->
    RecipeName = format_recipe_name(Cuisine, Recipe, Ingredients),
    IngredientStrs  = format_ingredients(Ingredients),
    InstructionStrs = format_instructions(Instructions),
    {RecipeName, IngredientStrs, InstructionStrs}.

pick_recipe(Recipes) ->
    util:pick_random(Recipes).

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
