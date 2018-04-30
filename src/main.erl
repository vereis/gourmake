-module(main).
-export([
    main/0
]).

-define(PRENOM, [
    "Dvorak's ",
    "Colemak's ",
    "Ada's ",
    "Apache's ",
    "Awk's ",
    "Bon's ",
    "Deb's ",
    "Jen's ",
    "Kerbe's ",
    "Lisa's ",
    "Linus' ",
    "Mac's ",
    "Pearl's ",
    "Ping's ",
    "Tux's ",
    "Alan's ",
    "Tim's ",
    "Lee's ",
    "Thompson's ",
    "Knuth's ",
    "Dennis' ",
    "Ritchie's ",
    "Gosling's ",
    "Edsger's ",
    "Kerninghan's ",
    "Brian's ",
    "Bjarne's ",
    "Stroustrup's ",
    "Backus' ",
    "Hoare's ",
    "Church's ",
    "Steve's ",
    "Naur's ",
    "Carmack's ",
    "Stallman's ",
    "RMS' ",
    "Munakata's "
]).

main() ->
    {Data, Recipes} = bootstrap:bootstrap(),
    {RecipeName, RecipeIngredients, RecipeInstructions} = parse_recipe(pick_recipe(Recipes), Data),
    pretty_print_list(RecipeName),
    pretty_print_list(RecipeIngredients),
    pretty_print_list(RecipeInstructions).

pretty_print_list(L) ->
    [io:format([Li | "~n"]) || Li <- L].

parse_recipe(Recipe, Data) ->
    Ingredients = parse_recipe_ingredients(Recipe, Data),
    Steps = parse_recipe_steps(Recipe, Ingredients),
    
    format_output(Recipe, Ingredients, Steps).

parse_recipe_ingredients(Recipe, Data) ->
    {Ingredients, Categories, Cuisines} = Data,
    #{ingredients := I, cuisines := C} = Recipe,

    maps:map(fun(_, {MinQ, MaxQ, Category}) ->
        Q = util:pick_random(lists:seq(MinQ, MaxQ)),
        RawData = [pick_from_category(util:pick_random(Category), util:pick_random(C), Cuisines, Categories) || _ <- lists:seq(1, Q)],
        lists:usort(RawData)
    end, I).

parse_recipe_name(Recipe, Ingredients) ->
    #{name := N} = Recipe,
    interpolate_recipe_string(N, Ingredients).

parse_recipe_steps(Recipe, Ingredients) ->
    #{steps := S} = Recipe,
    [interpolate_recipe_string(Step, Ingredients) || Step <- S]. 

interpolate_recipe_string(Str, Ingredients) ->
    {ok, Regex} = re:compile("\~([a-z]+)"),
    case re:run(Str, Regex, [global]) of
        nomatch -> {Str, []};
        {match, Matches} -> 
            ProcessedStr = binary_to_list(iolist_to_binary([re:replace(Str, Regex, "~s", [global])])),
            Flags = [list_to_existing_atom(string:slice(Str, M1, M2)) || [_, {M1, M2}] <- Matches],
            ProcessedFlags = [grammatical_concatenate(maps:get(F, Ingredients)) || F <- Flags],
            {ProcessedStr, ProcessedFlags}
    end.

grammatical_concatenate([H]) ->
    util:pretty_print_atom(H);
grammatical_concatenate([H, T]) ->
    string:join([util:pretty_print_atom(H), util:pretty_print_atom(T)], " and ");
grammatical_concatenate([H | T]) ->
    All = [util:pretty_print_atom(S) || S <- [H | T]],
    Last = util:pretty_print_atom(lists:last(T)),
    string:join([string:join(lists:droplast(All), ", "), Last], " and ").


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

format_recipe_name(Recipe, Ingredients) ->
    {RecipeName, RecipeLiterals} = parse_recipe_name(Recipe, Ingredients),
    [["# ", util:pick_random(?PRENOM), io_lib:format(RecipeName, RecipeLiterals)]].

format_output(Recipe, Ingredients, Instructions) ->
    RecipeName = format_recipe_name(Recipe, Ingredients),
    IngredientStrs  = format_ingredients(Ingredients),
    InstructionStrs = format_instructions(Instructions),
    {RecipeName, IngredientStrs, InstructionStrs}.

pick_recipe(Recipes) ->
    util:pick_random(Recipes).

pick_from_category(Category, Cuisine, CuisineData, Data) ->
    #{Category := Ingredients} = Data,

    % We typically want it to choose a ingredient of the correct cuisine, but not ALWAYS
    CuisineIngredients = lists:filter(fun(Ingredient) ->
        lists:member(Ingredient, Ingredients)
    end, maps:get(Cuisine, CuisineData)),

    Random = rand:uniform(100),
    Threshold = 100 - rand:uniform(75),

    case (Random > Threshold) and (length(CuisineIngredients) > 4) of
        true -> util:pick_random(CuisineIngredients);
        _    -> util:pick_random(Ingredients)
    end.

pick_from_cuisine(Cuisine, Data) ->
    #{Cuisine := Ingredients} = Data,
    util:pick_random(Ingredients).
