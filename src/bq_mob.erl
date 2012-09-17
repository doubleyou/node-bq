-module(bq_mob).

-include("bq.hrl").

-export([start_link/4]).
-export([init/1]).
-export([add/4]).

-record(mob, {
    id,
    area
}).

add(Id, Type, AreaId, {X, Y}) ->
    bq_mob_sup:add_mob([Id, Type, AreaId, {X, Y}]).

start_link(Id, Type, AreaId, {X, Y}) ->
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
    State = #mob{
        id = Id,
        area = AreaId
    },
    bq_actor:start_link(?MODULE, ActorState, State).

init(State) ->
    {ok, State}.
