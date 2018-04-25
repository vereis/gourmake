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
    parse_recipe(pick_recipe(Recipes), Data).

parse_recipe(Recipe, Data) ->
    Ingredients = parse_recipe_ingredients(Recipe, Data),
    Steps = parse_recipe_steps(Recipe, Ingredients),
    {RecipeName, RecipeLiterals} = parse_recipe_name(Recipe, Ingredients),

    io:format(" # " ++ util:pick_random(?PRENOM) ++ RecipeName, RecipeLiterals),
    io:format("   Ingredients:~n", []),
    IoIngredients = maps:to_list(Ingredients),
    lists:foreach(fun({_, Is}) ->
        lists:foreach(fun(I) ->
            io:format("     -  ~s~n", [util:pretty_print_atom(I)])
        end, Is)
    end, IoIngredients),

    io:format("   Instructions:~n", []),
    lists:foreach(fun(I) ->
        {TemplateString, TemplateLiterals} = lists:nth(I, Steps),
        io:format("     " ++ string:join([integer_to_list(I), TemplateString], ") "), TemplateLiterals)
    end, lists:seq(1, length(Steps))).

parse_recipe_ingredients(Recipe, Data) ->
    {Ingredients, Categories, Cuisines} = Data,
    #{ingredients := I} = Recipe,

    maps:map(fun(_, {Q, Category}) ->
        RawData = [pick_from_category(util:pick_random(Category), Categories) || I <- lists:seq(1, Q)],
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
        nomatch -> {Str ++ "~n", []};
        {match, Matches} -> 
            ProcessedStr = binary_to_list(iolist_to_binary([re:replace(Str, Regex, "~s", [global]), "~n"])),
            Flags = [list_to_existing_atom(string:slice(Str, M1, M2)) || [_, {M1, M2}] <- Matches],
            ProcessedFlags = [string:join([util:pretty_print_atom(I)|| I <- maps:get(F, Ingredients)], ", ") || F <- Flags],
            {ProcessedStr, ProcessedFlags}
    end.

pick_recipe(Recipes) ->
    util:pick_random(Recipes).

pick_from_category(Category, Data) ->
    #{Category := Ingredients} = Data,
    util:pick_random(Ingredients).

pick_from_cuisine(Cuisine, Data) ->
    #{Cuisine := Ingredients} = Data,
    util:pick_random(Ingredients).
