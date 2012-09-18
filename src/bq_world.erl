-module(bq_world).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include("bq.hrl").

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([cmd/1,
         broadcast/1,
         all_ids/0,
         total_players/0,
         uniq/0]).

-record(world, {
    height,
    width,
    collisions,
    doors,
    checkpoints,
    roaming_areas,
    chest_areas,
    static_chests,
    static_entities,
    titlesize
}).

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all_ids() ->
    ets:select(bq_actors, ets:fun2ms(fun(#actor{id=Id}) -> Id end)).

total_players() ->
    length(ets:select(bq_actors, ets:fun2ms(fun(#actor{type=warrior}) -> ok end))).

uniq() ->
    ets:update_counter(bq_util, uniq, 1).

broadcast(Msg) ->
    lists:foreach(
        fun(ID) ->
            catch bq_client:pid(ID) ! {json, Msg}
        end,
        ets:select(bq_actors, ets:fun2ms(fun(#actor{id = Id, type=warrior}) -> Id end))
    ).

cmd([check | _Ids]) ->
    %% FIXME: use ETS for checkpoints
    ok;
cmd([who | Ids]) ->
    RawActors = [bq_actor:lookup(Id) || Id <- Ids],
    [bq_actor:encode(A) || A <- RawActors];
cmd([zone | _]) ->
    [list | all_ids()];
cmd([aggro, _Id, _MobId]) ->
    ok;
cmd(Cmd) ->
    gen_server:call(?MODULE, Cmd).

%%
%% gen_server callbacks
%%

init(_) ->
    ets:new(bq_util, [public, named_table]),
    ets:new(bq_actors, [public, named_table, {keypos, 2}]),
    ets:new(bq_mobs_db, [public, named_table,{keypos,#property.type}]),
    ets:new(bq_names, [public, named_table, set]),

    ets:insert(bq_util, {uniq, 0}),

    load_mobs_db(),
    State = load_map(),
    load_mobs(State),

    {ok, State}.

handle_call([move, Id, X, Y], _From, World) ->
    %% TODO: actual map collisions should be here too
    case ets:select(bq_actors, ets:fun2ms(fun(#actor{id=Id_,x=X_,y=Y_}) when Id =/= Id_ andalso X =:= X_ andalso Y =:= Y_ -> ok end)) of
        [] ->
            ets:update_element(bq_actors, Id, [{#actor.x,X},{#actor.y,Y}]),
            broadcast([move, Id, X, Y]),
            {reply, ok, World};
        _ ->
            {reply, ok, World}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(load_mobs, State) ->
    %% FIXME: we should load mobs synchronously, before starting accepting 
    %%        messages
    spawn(fun() -> load_mobs(State#world.roaming_areas) end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

load_mobs_db() ->
    {ok,Bin} = file:read_file("node/server/js/properties.js"),
    {match, JSON} = re:run(Bin, "Properties = (\\{[^;]+\\});", [{capture,all_but_first,binary}]),
    {struct, Properties} = mochijson2:decode(re:replace(JSON, "(\\w+)\\:", "\"\\1\":", [global,noteol,{return,binary}])),
    lists:foreach(fun({Type, {struct,Props}}) ->
            Property = #property{drops = {struct,Drops}} = ?json2record(property,Props),
            ets:insert(bq_mobs_db, Property#property{
                type = binary_to_atom(Type,latin1),
                drops = [{binary_to_atom(T,latin1),V} || {T,V} <- Drops]
            })
        end,
        Properties
    ).

load_map() ->
    Path = "node/server/maps/world_server.json",
    {ok, Bin} = file:read_file(Path),
    {struct, Map} = mochijson2:decode(Bin),
    Width = proplists:get_value(<<"width">>, Map),
    Height = proplists:get_value(<<"height">>, Map),

    Collisions = [index_to_position(I, Width) || I <- proplists:get_value(<<"collisions">>, Map)],

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

    lager:info("Map loaded from ~s", [Path]),

     #world{
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
    }.

load_mobs(World = #world{roaming_areas=Areas, static_entities=StaticMobs, width=Width}) ->
    lists:foreach(
        fun(Area = #roaming_area{ type = Type, nb = N }) ->
            [bq_mob:add(uniq(), Type, random_area_position(Area, World))
                || _ <- lists:seq(1, N)]
        end,
        Areas
    ),
    lists:foreach(
        fun({RawCoord, Type}) ->
            bq_mob:add(uniq(), Type, index_to_position(RawCoord, Width))
        end,
        StaticMobs
    ).

index_to_position(Index, W) ->
    X = case Index rem W of
        0 -> W - 1;
        N -> N - 1
    end,
    Y = Index div W,
    {X, Y}.

random_area_position(Area = #roaming_area{x=X,y=Y,height=Height,width=Width}, World) ->
    XX = random:uniform(Width) + X,
    YY = random:uniform(Height) + Y,
    case collides({XX, YY}, World) of
        true -> random_area_position(Area, World);
        false -> {XX, YY}
    end.

collides(Coord, #world{collisions = Collisions}) ->
    lists:member(Coord, Collisions).
