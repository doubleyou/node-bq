-module(bq_world).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([
    add_character/1
    ,list_id/0
    ,spawns/1
]).

-include("bq.hrl").

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_character(Char) ->
    gen_server:call(?MODULE, {add_character, Char}).

list_id() ->
    gen_server:call(?MODULE, list_id).

spawns(SpawnIds) ->
    gen_server:call(?MODULE, {spawns, SpawnIds}).


%%
%% gen_server callbacks
%%

init(_) ->
    self() ! load_map,
    self() ! load_from_upstream,
    {ok, #world{}}.

handle_call({add_character, Char}, _From, State = #world{characters=Chars}) ->
    {reply, {ok, {16,233, 100}}, State#world{characters=[Char|Chars]}};
handle_call(list_id, _From, State = #world{entities = Entities}) ->
    {reply, [Id || #entity{id=Id} <- Entities], State};

handle_call(_Msg, _From, State) ->
    {reply, {invalid_call,_Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
    lager:info("Copy spawns from node upstream", []),
    {noreply, World#world{entities = Spawns}};

handle_info(_Info, State) ->
    {noreply, State}.

json(Cmd) -> {text, iolist_to_binary(mochijson2:encode(Cmd))}.

decode_spawn([2,Id,1,X,Y,Name,Orient,Armor,Weapon]) -> 
    #entity{id=Id,type=warrior,x=X,y=Y,orient=bq_http:orient_by_id(Orient),opts=[{name,Name},{armor,bq_http:entity_by_id(Armor)},{weapon,bq_http:entity_by_id(Weapon)}]};
decode_spawn([2,Id,Type,X,Y,Orient]) -> #entity{id=Id,type=bq_http:entity_by_id(Type),x=X,y=Y,orient=bq_http:orient_by_id(Orient)};
decode_spawn([2,Id,Type,X,Y]) -> #entity{id=Id,type=bq_http:entity_by_id(Type),x=X,y=Y}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
