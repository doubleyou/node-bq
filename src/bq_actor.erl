-module(bq_actor).
-behaviour(gen_server).

-include("bq.hrl").

-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([pid/1]).

-define(REGEN_INTERVAL, 2000).

-define(REGENERATE, regenerate).

%%
%% External API
%%

start_link(Module, ActorState, ModOptions) ->
    gen_server:start_link(?MODULE, [Module, ActorState, ModOptions], []).

pid(Id) ->
    gproc:where({n, l, {actor, Id}}).

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
                modstate = ModState
            },
            ets:insert(bq_actors, State),
            async_regenerate(),
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call(get, _From, State = #actor{ id = Id, x = X, y = Y, hp = HP }) ->
    {reply, [Id, X, Y, HP], State};
handle_call([hit, FromId, Dmg], _From, State = #actor{ hp = HP }) ->
    %% TODO: send damage/kill message
    NewHP = HP - Dmg,
    NewState = State#actor{hp = HP},
    case NewHP > 0 of
        true ->
            {reply, NewHP, NewState};
        false ->
            {stop, died, NewHP, State#actor{hp = NewHP}}
    end;
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
