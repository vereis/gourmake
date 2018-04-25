-module(util).
-export([
    pick_random/1,
    pretty_print_atom/1
]).

pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).

pretty_print_atom(Atom) ->
    Str = atom_to_list(Atom),
    string:join([string:titlecase(Lexeme) || Lexeme <- string:lexemes(Str, [$_])], " ").
