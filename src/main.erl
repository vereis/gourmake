-module(main).
-export([
    main/0
]).

-type bootstrap_result() :: ok
                          | {error, {shutdown, term()}}. % From supervisor:start_link/3

-spec main() -> ok.
main() ->
    ok = bootstrap(),

    % Generate recipe
    {RecipeName, {RecipeIngredients, RecipeInstructions}} = recipe_server:generate(),
    util:pretty_print_list(RecipeName),
    util:pretty_print_list(RecipeIngredients),
    util:pretty_print_list(RecipeInstructions),
 
    ok.

-spec bootstrap() -> bootstrap_result().
bootstrap() ->
    case gourmake_supervisor:start_link() of
        {ok, _} -> 
            ok;
        {error, {already_started, _}} -> 
            ok;
        Err ->
            Err
    end.
