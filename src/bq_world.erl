-module(bq_world).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([
    login/3
    ,list_id/0
    ,spawns/1
    ,entity/1
    ,move/3
    ,lootmove/2
    ,broadcast/1
    ,checkpoint/1
    ,attack/2
    ,hit/2
    ,random_pos/0
]).

-include("bq.hrl").

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Name, Armor, Weapon) ->
    gen_server:call(?MODULE, {login, Name, Armor, Weapon, self()}).

list_id() ->
    ets:select(?MODULE, ets:fun2ms(fun(#entity{id = Id}) -> Id end)).

spawns(SpawnIds) ->
    EntityList = [entity(Id) || Id <- SpawnIds],
    [encode_spawn(Entity) || #entity{} = Entity <- EntityList].

entity(Id) ->
    case ets:lookup(?MODULE, Id) of
        [E] -> E;
        [] -> undefined
    end.



broadcast(Msg) ->
    Pids = ets:select(?MODULE, ets:fun2ms(fun(#entity{pid = Pid}) when is_pid(Pid) -> Pid end)),
    [Pid ! {json,Msg} || Pid <- Pids, is_pid(Pid)],
    ok.


checkpoint(Id) ->
    gen_server:call(?MODULE, {checkpoint,Id}).

move(Id,X,Y) ->
    gen_server:call(?MODULE, {move,Id,X,Y}).

lootmove(Id, EntityId) ->
    gen_server:call(?MODULE, {lootmove, Id, EntityId}).


attack(AttackerId, EntityId) ->
    gen_server:call(?MODULE, {attack, AttackerId, EntityId}).

hit(EntityId, Damage) ->
    gen_server:call(?MODULE, {hit,EntityId,Damage}).

%%
%% gen_server callbacks
%%

init(_) ->
    self() ! load_properties,
    self() ! load_map,
    self() ! load_from_upstream,
    ets:new(bq_properties, [public, named_table,{keypos,#property.type}]),
    ets:new(?MODULE, [public,named_table,{keypos,#entity.id}]),
    %% Let's make uniqs greater than 10000
    ets:insert(?MODULE, {uniq, 10000}),
    {ok, #world{}}.

unique_id() ->
    gen_server:call(?MODULE, uniq).

random_pos() ->
    gen_server:call(?MODULE, random_pos).

random_pos(#world{width = W, height = H, collisions = Collisions} = World) ->
    N = random:uniform(W*H),
    case lists:member(N, Collisions) of
        true ->
            random_pos(World);
        false ->
            X = N rem W,
            Y = N div W,
            case ets:select(?MODULE, ets:fun2ms(fun(#entity{x = X_, y = Y_}) when X_ == X, Y_ == Y -> true end)) of
                [] -> {X,Y};
                [_|_] -> random_pos(World)
            end
    end.

handle_call(uniq, _From, World) ->
    {reply, ets:update_counter(?MODULE, uniq, 1), World};
handle_call(random_pos, _From, #world{} = World) ->
    {reply, random_pos(World), World};

handle_call({login, Name, Armor, Weapon, Pid}, _From, State = #world{}) ->
    erlang:monitor(process,Pid),
    Entity = case ets:select(?MODULE, ets:fun2ms(fun(#entity{name = N} = E) when N == Name -> E end)) of
        [#entity{x = X, y = Y, hitpoints = HP} = Entity_] -> Entity_#entity{pid = Pid};
        [] ->
            % {X,Y} = random_pos(State),
            {X,Y} = {16,210},
            Entity_ = #entity{
                id = unique_id(),
                name = Name,
                type = warrior,
                orient = down,
                hitpoints = HP = 100,
                armor = Armor,
                weapon = Weapon,
                x = X,
                y = Y,
                pid = Pid
            },
            % TODO: send spawn message
            Entity_
    end,
    #entity{id=Id} = Entity,
    
    ets:insert(?MODULE, Entity),
    broadcast(encode_spawn(Entity)),
    {reply, {ok, {Id,X,Y, HP}}, State};
    
handle_call({checkpoint,Id}, _From, #world{checkpoints = Checkpoints} = World) ->
    Reply = case lists:keyfind(Id,#checkpoint.id,Checkpoints) of
        #checkpoint{} = Checkpoint -> Checkpoint;
        false -> undefined
    end,
    {reply, Reply, World};

handle_call({move,Id,X,Y}, _From, #world{} = World) ->
    case ets:update_element(?MODULE, Id, [{#entity.x,X},{#entity.y,Y}]) of
        true ->
            broadcast([move, Id, X, Y]),
            {reply, ok, World};
        false ->
            {reply, {error, no_entity}, World}
    end;    

handle_call({lootmove, Id, EntityId}, _From, #world{} = World) ->
    case ets:lookup(?MODULE, Id) of
        [#entity{} = _Player] -> ok;
        [] -> throw({reply, {error, no_player}, World})
    end,
    
    case ets:lookup(?MODULE, EntityId) of
        [#entity{x=X, y=Y}] ->
            broadcast([lootmove, Id, EntityId]),
            ets:delete(?MODULE, EntityId),
            ets:update_element(?MODULE, Id, [{#entity.x,X},{#entity.y,Y}]),
            {reply, ok, World};
        false ->
            {reply, {error, no_entity}, World}
    end;


handle_call({attack, AttackerId, EntityId}, _From, #world{} = World) ->
    case entity(EntityId) of
        #entity{haters = Haters} = Entity ->
            ets:insert(?MODULE, Entity#entity{haters = [AttackerId|Haters]}),
            {reply, ok, World};
        undefined ->
            {reply, {error, no_entity}, World}
    end;

handle_call({hit,EntityId,Damage}, _From, #world{} = World) ->
    case entity(EntityId) of
        undefined ->
            {reply, {error, no_entity}, World};
        #entity{hitpoints = HP} when HP - Damage > 0 ->
            ets:update_element(?MODULE, EntityId, {#entity.hitpoints, HP - Damage}),
            {reply, {ok, damage}, World};
        #entity{x=X,y=Y,hitpoints = HP, type = Type, haters = Haters} when HP - Damage =< 0 ->
            [#property{drops = Drops}] = ets:lookup(bq_properties, Type),
            {Drop,_Percentage} = lists:nth(random:uniform(length(Drops)), Drops),
            DropItem = #entity{
                x = X,
                y = Y,
                type = Drop,
                id = DropId = unique_id()
            },
            ets:insert(?MODULE, DropItem),
            broadcast([[kill, bq_msg:entity_by_name(Type)], [despawn, EntityId],[drop,EntityId,DropId,bq_msg:entity_by_name(Drop),Haters]]),
            ets:delete(?MODULE, EntityId),
            lager:debug("Deleting entity ~p(~p) with ~p hitpoints", [Type,EntityId,HP - Damage]),
            {reply, {ok,killed}, World}
    end;


handle_call(_Msg, _From, State) ->
    {reply, {invalid_call,_Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(load_properties, #world{} = World) ->
    {ok,Bin} = file:read_file("node/server/js/properties.js"),
    {match, JSON} = re:run(Bin, "Properties = (\\{[^;]+\\});", [{capture,all_but_first,binary}]),
    {struct, Properties} = mochijson2:decode(re:replace(JSON, "(\\w+)\\:", "\"\\1\":", [global,noteol,{return,binary}])),
    lists:foreach(fun({Type, {struct,Props}}) ->
        Property = #property{drops = {struct,Drops}} = ?json2record(property,Props),
        ets:insert(bq_properties, Property#property{
            type = binary_to_atom(Type,latin1),
            drops = [{binary_to_atom(T,latin1),V} || {T,V} <- Drops]
        })
    end, Properties),
    {noreply, World};
    

handle_info(load_map, #world{} = State) ->
    Path = "node/server/maps/world_server.json",
    {ok, Bin} = file:read_file(Path),
    {struct, Map} = mochijson2:decode(Bin),
    Width = proplists:get_value(<<"width">>, Map),
    Height = proplists:get_value(<<"height">>, Map),
    Collisions = proplists:get_value(<<"collisions">>, Map),
    Doors = lists:map(fun({struct, D}) ->
        ?json2record(door, D)
    end, proplists:get_value(<<"doors">>, Map)),
    
    Checkpoints = lists:map(fun({struct, D}) ->
        ?json2record(checkpoint, D)
    end, proplists:get_value(<<"checkpoints">>, Map)),

    RoamingAreas = lists:map(fun({struct, D}) ->
        Area = #roaming_area{type = T} = ?json2record(roaming_area, D),
        Area#roaming_area{type = binary_to_atom(T,latin1)}
    end, proplists:get_value(<<"roamingAreas">>, Map)),
    
    ChestAreas = lists:map(fun({struct, D}) ->
        ?json2record(chest_area, D)
    end, proplists:get_value(<<"chestAreas">>, Map)),

    Chests = lists:map(fun({struct, D}) ->
        ?json2record(chest, D)
    end, proplists:get_value(<<"staticChests">>, Map)),
    
    {struct, StaticEnt} = proplists:get_value(<<"staticEntities">>, Map),
    StaticEntities = lists:map(fun({Id, Type}) ->
        {list_to_integer(binary_to_list(Id)), binary_to_atom(Type,latin1)}
    end, StaticEnt),
    
    State1 = State#world{
        width = Width,
        height = Height,
        collisions = Collisions,
        doors = Doors,
        checkpoints = Checkpoints,
        roaming_areas = RoamingAreas,
        chest_areas = ChestAreas,
        static_chests = Chests,
        static_entities = StaticEntities,
        titlesize = proplists:get_value(<<"titlesize">>, Map)
    },
    lager:info("Map loaded from ~s", [Path]),
    {noreply, State1};

handle_info(load_from_upstream, #world{} = World) ->
    {ok, Socket} = websocket_client:start_link("http://localhost:8001/", websocket_client, self()),
    {text, <<"go">>} = websocket_client:read(Socket),
    websocket_client:call(Socket, json([0,<<"Mp3">>,21,60])),
    {text,R1} = websocket_client:read(Socket),
    [_,[19|List]] = mochijson2:decode(R1),
    {text,R2} = websocket_client:call(Socket, json([20|List])),
    Spawns = [decode_spawn(S) || S <- mochijson2:decode(R2)],
    ets:insert(?MODULE, Spawns),
    lager:info("Copy spawns from node upstream", []),
    {noreply, World};

handle_info(_Info, State) ->
    {noreply, State}.

json(Cmd) -> {text, iolist_to_binary(mochijson2:encode(Cmd))}.

decode_spawn([2,Id,1,X,Y,Name,Orient,Armor,Weapon]) -> 
    #entity{id=Id,type=warrior,x=X,y=Y,orient=bq_msg:orient_by_id(Orient),name=Name,armor=bq_msg:entity_by_id(Armor),weapon=bq_msg:entity_by_id(Weapon)};
decode_spawn([2,Id,Type,X,Y,Orient]) ->
    T = bq_msg:entity_by_id(Type),
    [#property{hp = HP}] = ets:lookup(bq_properties, T),
    #entity{id=Id,type=T,hitpoints = HP,x=X,y=Y,orient=bq_msg:orient_by_id(Orient)};
decode_spawn([2,Id,Type,X,Y]) -> #entity{id=Id,type=bq_msg:entity_by_id(Type),x=X,y=Y}.


encode_spawn(#entity{type=Type,x=X,y=Y,id=Id,orient=Orient,name=Name,armor=Armor,weapon=Weapon}) ->
    [2,Id,Type,X,Y] ++ if
        Type == warrior ->
            [Name,Orient,Armor,Weapon];
        Orient =/= undefined ->
            [Orient];
        true ->
            []
    end.    



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
