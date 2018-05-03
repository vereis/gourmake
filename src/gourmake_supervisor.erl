-module(gourmake_supervisor).
-behaviour(supervisor).

-export([
    init/1
]).

-export([
    start_link/0
]).

-spec start_link() -> ignore | {error ,_} | {ok ,pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Flags = #{
      strategy => one_for_one,
      intensity => 1,
      period => 5
    },
    ChildSpecs = [
        #{
            id => rserve,
            start => {recipe_server, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [recipe_server]
        },
        #{
            id => iserve,
            start => {ingredient_server, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [ingredient_server]
        },
        #{
            id => nserve,
            start => {name_server, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [name_server]
        }
    ],
    {ok, {Flags, ChildSpecs}}.
