-module(bootstrap).
-export([
    bootstrap/0
]).

bootstrap() ->
    Data = load_data(<<"../data/ingredients.term">>),
    Recipes = load_data(<<"../data/recipes.term">>),
    Names = load_data(<<"../data/names.term">>),

    {parse_ingredients(Data), Recipes, Names}.

load_data(Path) ->
    {ok, [Data]} = file:consult(Path),
    Data.

parse_ingredients(Ingredients) ->
    lists:foldl(fun({Ingredient, IngredientProperties}, {IngAcc, CatAcc, CuiAcc}) ->
        #{categories := Categories, cuisines := Cuisines} = IngredientProperties,
        
        UpdatedIngAcc = maps:put(Ingredient, {
                                   util:pretty_print_atom(Ingredient), 
                                   Categories, Cuisines}, IngAcc),

        UpdatedCatAcc = lists:foldl(fun(Category, Acc) ->
            case maps:is_key(Category, Acc) of
                true ->
                    maps:update(Category, [Ingredient | maps:get(Category, Acc)], Acc);
                _    ->
                    maps:put(Category, [Ingredient], Acc)
            end
        end, CatAcc, Categories),

        UpdatedCuiAcc = lists:foldl(fun(Cuisine, Acc) ->
            case maps:is_key(Cuisine, Acc) of
                true ->
                    maps:update(Cuisine, [Ingredient | maps:get(Cuisine, Acc)], Acc);
                _    ->
                    maps:put(Cuisine, [Ingredient], Acc)
            end
        end, CuiAcc, Cuisines),

        {UpdatedIngAcc, UpdatedCatAcc, UpdatedCuiAcc}
    end, {#{}, #{}, #{}}, Ingredients).
