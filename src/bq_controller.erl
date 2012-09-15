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
    % ,attack/2
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
    NewState = State#client{
        id = Id,
        logged_in = true
    },
    lager:info("Client with id ~p connected", [Id]),

    % server/js/player.js:65
    Welcome = [welcome, Id, Name, X, Y, Hitpoints],
    % server/js/worldserver.js:853
    Population = [population, 2, null],
    List = [list|bq_world:list_id()],
    self() ! {json, [Population, List]},
    {reply, Welcome, NewState}.

who(SpawnIds, State) ->
    % server/js/worldserver.js:249
    {reply, bq_world:spawns(SpawnIds), State}.

move([X, Y], State) ->
    {reply, [4, State#client.id, X, Y], State#client{x = X, y = Y}}.

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
