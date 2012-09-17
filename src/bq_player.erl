-module(bq_player).

-include("bq.hrl").

-export([start_link/4]).

-export([init/1]).

-export([by_name/1]).

by_name(Name) ->
    bq_actor:pid(id_by_name(Name)).

start_link(Pid, Name, Armor, Weapon) ->
    ActorState = case bq_world:lookup_actor(id_by_name(Name)) of
        undefined ->
            Id = bq_world:uniq(),
            #actor{
                id = Id,
                type = warrior
            };
        AS ->
            AS
    end,
    State = #player{
        id = ActorState#actor.id,
        name = Name,
        client_pid = Pid,
        armor = Armor,
        weapon = Weapon
    },
    bq_actor:start_link(?MODULE, ActorState, State).

%%
%% Behaviour callbacks
%%

init(State) ->
    gproc:reg({n, l, {player, State#player.name}}, State#player.id),
    {ok, State}.

%%
%% Internal functions
%%

id_by_name(Name) ->
    try gproc:lookup_value({n, l, {player, Name}})
    catch error:badarg ->
        undefined
    end.
