-module(bq_controller).


-include("bq.hrl").


-export([handle/2]).


-export([hello/2,
         who/2,
         move/2,
         chat/2]).



handle([Command|Args], State) ->
    case erlang:function_exported(?MODULE, Command, 2) of
        true ->
            Reply = ?MODULE:Command(Args, State),
            Reply;
        false ->
            {noreply, State}
    end.







hello([Name | _], State) ->
    ID = erlang:phash2(Name, 100000),
    NewState = State#state{
        id = ID,
        logged_in = true
    },
    lager:info("Client with id ~p connected", [ID]),
    % gproc:reg({n, l, ID}),
    {ok, {X,Y,Hitpoints}} = bq_world:add_character(ID),
    % server/js/player.js:65
    Welcome = [welcome, NewState#state.id, Name, X, Y, Hitpoints],
    % server/js/worldserver.js:853
    Population = [population, 2, null],
    List = [list|bq_world:list_id()],
    self() ! {json, [Population, List]},
    {reply, Welcome, NewState}.

who(Spawns, State) ->
    % server/js/worldserver.js:249
    {noreply, State}.

move([X, Y], State) ->
    {reply, [4, State#state.id, X, Y], State#state{x = X, y = Y}}.

chat(Cmd, State) ->
    lager:info("Chat message: ~p", [Cmd]),
    {reply, [], State}.

