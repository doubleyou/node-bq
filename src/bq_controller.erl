-module(bq_controller).


-include("bq.hrl").


-export([handle/2]).


-export([
    hello/2
    ,who/2
    ,move/2
    ,chat/2
    ,check/2
    ,lootmove/2
    ,attack/2
    ,hit/2
    ,hurt/2
    ,zone/2
]).



handle([Command|Args], State) ->
    case erlang:function_exported(?MODULE, Command, 2) of
        true ->
            Reply = ?MODULE:Command(Args, State),
            Reply;
        false ->
            {noreply, State}
    end.







hello([Name | _], #client{} = State) ->
    % gproc:reg({n, l, ID}),
    {ok, {Id, X,Y,Hitpoints}} = bq_world:login(Name),

    lager:info("Client with id ~p connected", [Id]),

    % server/js/player.js:65
    Welcome = [welcome, Id, Name, X, Y, Hitpoints],
    % server/js/worldserver.js:853
    Population = [population, 2, null],
    List = [list|bq_world:list_id()],
    self() ! {json, [Population, List]},
    {reply, Welcome, State#client{
        id = Id,
        logged_in = true,
        x = X,
        y = Y,
        hitpoints = Hitpoints
    }}.

zone([], Client) ->
    {reply, [list|bq_world:list_id()], Client}.

who(SpawnIds, State) ->
    % server/js/worldserver.js:249
    {reply, bq_world:spawns(SpawnIds), State}.

move([X, Y], #client{id = Id} = State) ->
    ok = bq_world:move(Id, X, Y),
    {noreply, State#client{x = X, y = Y}}.

chat(Cmd, State) ->
    lager:info("Chat message: ~p", [Cmd]),
    {reply, [], State}.


check([Id], State) ->
    case bq_world:checkpoint(Id) of
        #checkpoint{} = CheckPoint ->
            {noreply, State#client{checkpoint = CheckPoint}};
        undefined ->
            {noreply, State}
    end.


% server/js/player.js:99
lootmove([X,Y,EntityId], #client{id = Id} = Client) ->
    % TODO: add broadcast lootmove
    ok = bq_world:lootmove(Id, EntityId),
    {noreply, Client#client{x = X, y = Y}}.


attack([EntityId], #client{id = Id} = Client) ->
    % bq_world:broadcast([attack, Id, EntityId]),
    bq_world:attack(Id, EntityId),
    {reply, [attack, EntityId, Id], Client}.

hit([EntityId], #client{} = Client) ->
    case bq_world:entity(EntityId) of
        #entity{} = Entity ->
            Damage = damage(weapon_level(Client), armor_level(Entity)),
            % server/js/player.js:131, add hurt and hate
            case bq_world:hit(EntityId, Damage) of
                {ok, damage} ->
                    {reply, [damage, EntityId, Damage], Client};
                {ok, killed} ->
                    {reply, [damage, EntityId, Damage], Client}
            end;
        undefined ->
            {noreply, Client}
    end.


hurt([EntityId], #client{hitpoints = HP1, id = Id} = Client) ->
    case bq_world:entity(EntityId) of
        #entity{} = Entity ->
            Damage = damage(weapon_level(Entity), armor_level(Client)),
            HP2 = HP1 - Damage,
            ets:update_element(bq_world, Id, {#entity.hitpoints, HP2}),
            {reply, [health, HP2, 1], Client#client{hitpoints = HP2}};
        undefined ->
            {noreply, Client}
    end.

            

armor_level(#entity{type = Type}) ->
    case ets:lookup(bq_properties, Type) of
        [#property{armor = Armor}] -> Armor;
        [] -> undefined
    end;

armor_level(#client{}) ->
    3.


% FIXME
% shared/js/gametypes.js:162
weapon_level(#client{}) ->
    2;

weapon_level(#entity{}) ->
    1.



damage(Weapon, Armor) when is_number(Weapon) andalso is_number(Armor) ->
    Dealt = Weapon * (random:uniform(5) + 2),
    Absorbed = Armor * (random:uniform(2) + 1),
    Damage = Dealt - Absorbed,
    if Damage < 0 -> random:uniform(3);
    true -> Damage end.

        






