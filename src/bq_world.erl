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

%%
%% gen_server callbacks
%%

init(_) ->
    self() ! load_map,
    {ok, #world{}}.

handle_call({add_character, Char}, _From, State = #world{characters=Chars}) ->
    {reply, {ok, {16,233, 100}}, State#world{characters=[Char|Chars]}};
handle_call(list_id, _From, State = #world{}) ->
    {reply, [926,927,928,1120,1121,1122,1220,1221,1222,1820,1821,11920,11921,12220,57134,867220], State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

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

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
