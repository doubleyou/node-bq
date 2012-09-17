-module(bq_player).

-include("bq.hrl").

-export([start_link/4]).

-export([init/1]).

-export([by_name/1]).

by_name(Name) ->
    bq_actor:pid(id_by_name(Name)).

start_link(Pid, Name, Armor, Weapon) ->
    ActorState = case bq_actor:lookup(id_by_name(Name)) of
        undefined ->
            Id = bq_world:uniq(),
            ets:insert_new(bq_names, {Name, Id}),
            #actor{
                id = Id,
                type = warrior,
                armor = Armor,
                weapon = Weapon
            };
        AS ->
            AS
    end,
    State = #player{
        name = Name,
        client_pid = Pid
    },
    bq_actor:start_link(?MODULE, ActorState, State).

%%
%% Behaviour callbacks
%%

init(State) ->
    {ok, State}.

%%
%% Internal functions
%%

id_by_name(Name) ->
    case ets:lookup(bq_names, Name) of
        [{Name, Id}] -> Id;
        [] -> undefined
    end.
