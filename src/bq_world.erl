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
    login/1
    ,list_id/0
    ,spawns/1
    ,entity/1
    ,move/3
    ,lootmove/2
    ,broadcast/1
    ,checkpoint/1
]).

-include("bq.hrl").

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Name) ->
    gen_server:call(?MODULE, {login, Name, self()}).

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
    Pids = ets:select(ets:fun2ms(fun(#entity{pid = Pid}) when is_pid(Pid) -> Pid end)),
    [Pid ! {json,Msg} || Pid <- Pids, is_pid(Pid)],
    ok.


checkpoint(Id) ->
    gen_server:call(?MODULE, {checkpoint,Id}).

move(Id,X,Y) ->
    gen_server:call(?MODULE, {move,Id,X,Y}).

lootmove(Id, EntityId) ->
    gen_server:call(?MODULE, {lootmove, Id, EntityId}).

%%
%% gen_server callbacks
%%

init(_) ->
    self() ! load_properties,
    self() ! load_map,
    self() ! load_from_upstream,
    ets:new(bq_properties, [public, named_table,{keypos,#property.type}]),
    ets:new(?MODULE, [public,named_table,{keypos,#entity.id}]),
    {ok, #world{}}.

handle_call({login, Name, Pid}, _From, State = #world{}) ->
    erlang:monitor(process,Pid),
    Entity = case ets:select(?MODULE, ets:fun2ms(fun(#entity{name = N} = E) when N == Name -> E end)) of
        [#entity{} = Entity_] -> Entity_#entity{pid = Pid};
        [] -> 
            Entity_ = #entity{
                id = erlang:phash2(Name, 100000),
                x = 16,
                y = 233
            },
            % TODO: send spawn message
            Entity_
    end,
    #entity{id=Id,x=X,y=Y} = Entity,
    ets:insert(?MODULE, Entity),
    {reply, {ok, {Id,X,Y, 100}}, State};
    
handle_call({checkpoint,Id}, _From, #world{checkpoints = Checkpoints} = World) ->
    Reply = case lists:keyfind(Id,#checkpoint.id,Checkpoints) of
        #checkpoint{} = Checkpoint -> Checkpoint;
        false -> undefined
    end,
    {reply, Reply, World};

handle_call({move,Id,X,Y}, _From, #world{} = World) ->
    case ets:update_element(?MODULE, Id, [{#entity.x,X},{#entity.y,Y}]) of
        true ->
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
    #entity{id=Id,type=warrior,x=X,y=Y,orient=bq_http:orient_by_id(Orient),name=Name,armor=bq_http:entity_by_id(Armor),weapon=bq_http:entity_by_id(Weapon)};
decode_spawn([2,Id,Type,X,Y,Orient]) ->
    T = bq_http:entity_by_id(Type),
    [#property{hp = HP}] = ets:lookup(bq_properties, T),
    #entity{id=Id,type=T,hitpoints = HP,x=X,y=Y,orient=bq_http:orient_by_id(Orient)};
decode_spawn([2,Id,Type,X,Y]) -> #entity{id=Id,type=bq_http:entity_by_id(Type),x=X,y=Y}.


encode_spawn(#entity{type=Type,x=X,y=Y,id=Id,orient=Orient,name=Name,armor=Armor,weapon=Weapon}) ->
    [2,Id,bq_http:entity_by_name(Type),X,Y] ++ if
        Type == warrior ->
            [Name,bq_http:orient_by_name(Orient),bq_http:entity_by_name(Armor),bq_http:entity_by_name(Weapon)];
        Orient =/= undefined ->
            [bq_http:orient_by_name(Orient)];
        true ->
            []
    end.    



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
