-module(bq_mob).

-include("bq.hrl").

-export([start_link/3]).
-export([init/1]).
-export([add/3]).

-record(mob, {
    aggro = []
}).

add(Id, Type, {X, Y}) ->
    bq_mob_sup:add_mob([Id, Type, {X, Y}]).

start_link(Id, Type, {X, Y}) ->
    [Props] = ets:lookup(bq_mobs_db, Type),
    HP = Props#property.hp,
    Armor = Props#property.armor,
    Weapon = Props#property.weapon,
    ActorState = #actor{
        id = Id,
        type = Type,
        x = X,
        y = Y,
        hp = HP,
        armor = Armor,
        weapon = Weapon
    },
    State = #mob{},
    bq_actor:start_link(?MODULE, ActorState, State).

init(State) ->
    {ok, State}.
