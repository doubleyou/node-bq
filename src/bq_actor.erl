-module(bq_actor).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include("bq.hrl").

-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([pid/1,
         area/1,
         encode/1,
         lookup/1]).

-define(REGEN_INTERVAL, 2000).

-define(REGENERATE, regenerate).

%%
%% External API
%%

start_link(Module, ActorState, ModOptions) ->
    gen_server:start_link(?MODULE, [Module, ActorState, ModOptions], []).

pid(Id) ->
    gproc:where({n, l, {actor, Id}}).

lookup(Id) ->
    case ets:lookup(bq_actors, Id) of
        [Actor] -> Actor;
        [] -> undefined
    end.

encode(#actor{type=Type,x=X,y=Y,id=Id,orientation=Orient,modstate=MS,armor=Armor,weapon=Weapon}) ->
    [spawn, Id, Type, X, Y] ++ if
        Type =:= warrior ->
            [MS#player.name, Orient, Armor, Weapon];
        Orient =/= undefined ->
            [Orient];
        true ->
            []
    end.

area({X, Y}) ->
    {(X-1) div 28, (Y-1) div 12}.

%%
%% gen_server callbacks
%%

init([Module, ActorState, ModOptions]) ->
    process_flag(trap_exit, true),
    Id = ActorState#actor.id,
    gproc:reg({n, l, {actor, Id}}, self()),
    case Module:init(ModOptions) of
        {ok, ModState} ->
            State = ActorState#actor {
                module = Module,
                pid = self(),
                modstate = ModState,
                area = area({ActorState#actor.x, ActorState#actor.y})
            },
            ets:insert(bq_actors, State),
            async_regenerate(),
            bq_world:broadcast(encode(State)),
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call([teleport, X, Y], _From, State = #actor{id = Id}) ->
    bq_world:broadcast([teleport, Id, X, Y]),
    handle_call([zone], _From, State#actor{x=X, y=Y});
handle_call([move, X, Y], _From, State = #actor{id = Id}) ->
    bq_world:broadcast([move, Id, X, Y]),
    {reply, ok, State#actor{x=X, y=Y}};
handle_call([loot, ItemId], _From, State) ->
    %% TODO: update state, despawn the item
    {reply, ok, State};
handle_call([zone], _From, State = #actor{ x = X, y = Y, id = Id }) ->
    NewArea = area({X, Y}),
    lager:info("~p ~p", [{X, Y}, NewArea]),
    ets:update_element(bq_actors, Id, [{#actor.area, NewArea}]),
    Ids = bq_world:area_ids(NewArea),
    {reply, [list | Ids], State#actor{area=NewArea}};
handle_call(get, _From, State = #actor{ id = Id, x = X, y = Y, hp = HP }) ->
    {reply, [Id, X, Y, HP], State};

handle_call([hurt, TargetId], _From, State = #actor{id=Id,armor=Armor,hp=HP}) ->
    Weapon = gen_server:call(pid(TargetId), weapon),
    Dmg = damage(Weapon, Armor),
    NewHP = HP - Dmg,
    case NewHP > 0 of
        true ->
            {reply, [health, NewHP], State#actor{hp = NewHP}};
        false ->
            bq_world:broadcast([[damage, Id, Dmg], [kill, Id], [despawn, Id]]),
            %% TODO: Respawn here
            {stop, {died, NewHP}, normal, State}
    end;

handle_call([npchurt, Weapon], _From, State = #actor{id=Id,armor=Armor,hp=HP,type=Type}) ->
    Dmg = damage(Weapon, Armor),
    NewHP = HP - Dmg,
    case NewHP > 0 of
        true ->
            {reply, Dmg, State#actor{hp = NewHP}};
        false ->
            bq_world:broadcast([[damage, Id, Dmg], [kill, bq_msg:entity_by_name(Type)], [despawn, Id]]),
            %% TODO: Respawn here
            {stop, normal, Dmg, State}
    end;

handle_call([hit, TargetId], _From, State = #actor{weapon=Weapon}) ->
    Pid = pid(TargetId),
    Damage = gen_server:call(Pid, [npchurt, Weapon]),
    {reply, [damage, TargetId, Damage], State};

handle_call(weapon, _From, State = #actor{weapon = Weapon}) ->
    {reply, Weapon, State};

handle_call(Msg, From, State = #actor{module = Module, modstate = ModState}) ->
    case Module:handle_call(Msg, From, ModState) of
        {reply, Reply, NewModState} ->
            {reply, Reply, State#actor{modstate=NewModState}};
        {noreply, NewModState} ->
            {noreply, State#actor{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#actor{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#actor{modstate=NewModState}}
    end.

handle_cast(Msg, State = #actor{module = Module, modstate = ModState}) ->
    case Module:handle_cast(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#actor{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#actor{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#actor{modstate=NewModState}}
    end.

handle_info(?REGENERATE, State = #actor{ hp = HP, max_hp = HP }) ->
    %% FIXME: maybe start regeneration only when some damage is dealt
    %%        and not send regen message until then.
    async_regenerate(),
    {noreply, State};
handle_info(?REGENERATE, State = #actor{ hp = HP, regen_rate = Reg }) ->
    async_regenerate(),
    %% TODO: Send regen message
    {noreply, State#actor{hp = HP + Reg}};
handle_info(Info, State = #actor{module = Module, modstate = ModState}) ->
    case Module:handle_info(Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#actor{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#actor{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#actor{modstate=NewModState}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

async_regenerate() ->
    timer:send_after(self(), ?REGEN_INTERVAL, ?REGENERATE).

damage(Weapon0, Armor0) ->
    Weapon = bq_msg:weapon_lvl(Weapon0),
    Armor = bq_msg:armor_lvl(Armor0),
    Dealt = Weapon * (random:uniform(5) + 2),
    Absorbed = Armor * (random:uniform(2) + 1),
    Damage = Dealt - Absorbed,
    if Damage < 0 -> random:uniform(3);
    true -> Damage end.
