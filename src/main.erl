-module(main).
-export([
    main/0
]).

main() ->
    {ok, _} = bootstrap(),
    
    % Generate recipe
    {RecipeName, {RecipeIngredients, RecipeInstructions}} = recipe_server:generate(),
    util:pretty_print_list(RecipeName),
    util:pretty_print_list(RecipeIngredients),
    util:pretty_print_list(RecipeInstructions).

%% Start servers if neccessary so that we can generate recipes
bootstrap() -> 
    {R1, IngredientServer} = case is_pid(P1 = whereis(ingredient_server)) of
        true -> {ok, P1};
        _    -> ingredient_server:start_link()
    end,
    {R2, RecipeServer} = case is_pid(P2 = whereis(recipe_server)) of
        true -> {ok, P2};
        _    -> recipe_server:start_link()
    end,
    {R3, NameServer} = case is_pid(P3 = whereis(name_server)) of
        true -> {ok, P3};
        _    -> name_server:start_link()
    end,

    case lists:all(fun(X) -> X =:= ok end, [R1, R2, R3]) of
        true -> {ok, {IngredientServer, RecipeServer, NameServer}};
        _    -> {err, {start_link_fail, [R1, R2, R3]}}
    end.
