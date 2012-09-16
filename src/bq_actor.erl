-module(bq_actor).
-behaviour(gen_server).

-export([start_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([lookup/1]).
-export([move/2,
         damage/3]).

-define(REGEN_INTERVAL, 2000).

-define(REGENERATE, bq_actor_regenerate).
-define(MOVE, bq_actor_move).
-define(DAMAGE, bq_actor_damage).

-record(state, {
    module,
    id,
    x = 0,
    y = 0,
    orientation = random:uniform(4),
    hp = 100,
    max_hp = 100,
    regen_rate = 1,
    modstate
}).

%%
%% External API
%%

start_link(Module, Id, ActorOptions, ModOptions) ->
    gen_server:start_link(?MODULE, [Module, Id, ActorOptions, ModOptions], []).

move(Id, {X, Y}) ->
    gen_server:call(lookup(Id), {?MOVE, X, Y}).

damage(Id, From, Dmg) ->
    gen_server:call(lookup(Id), {?DAMAGE, From, Dmg}).

%%
%% gen_server callbacks
%%

init([Module, Id, ActorOptions, ModOptions]) ->
    process_flag(trap_exit, true),
    case Module:init(ModOptions) of
        {ok, ModState} ->
            State = parse_options(ActorOptions, #state {
                module = Module,
                id = Id,
                modstate = ModState
            }),
            async_regenerate(),
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({?MOVE, X, Y}, _From, State) ->
    {reply, ok, State#state{x = X, y = Y}};
handle_call({?DAMAGE, FromId, Dmg}, _From, State = #state{ hp = HP }) ->
    %% TODO: send damage/kill message
    NewHP = HP - Dmg,
    NewState = State#state{hp = HP},
    case NewHP > 0 of
        true ->
            {reply, NewHP, NewState};
        false ->
            {stop, died, NewHP, State#state{hp = NewHP}}
    end;
handle_call(Msg, From, State = #state{module = Module, modstate = ModState}) ->
    case Module:handle_call(Msg, From, ModState) of
        {reply, Reply, NewModState} ->
            {reply, Reply, State#state{modstate=NewModState}};
        {noreply, NewModState} ->
            {noreply, State#state{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#state{modstate=NewModState}}
    end.

handle_cast(Msg, State = #state{module = Module, modstate = ModState}) ->
    case Module:handle_cast(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#state{modstate=NewModState}}
    end.

handle_info(?REGENERATE, State = #state{ hp = HP, max_hp = HP }) ->
    %% FIXME: maybe start regeneration only when some damage is dealt
    %%        and not send regen message until then.
    async_regenerate(),
    {noreply, State};
handle_info(?REGENERATE, State = #state{ hp = HP, regen_rate = Reg }) ->
    async_regenerate(),
    %% Send regen message
    {noreply, State#state{hp = HP + Reg}};
handle_info(Info, State = #state{module = Module, modstate = ModState}) ->
    case Module:handle_info(Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#state{modstate=NewModState}};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#state{modstate=NewModState}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

lookup(Id) ->
    Id.

parse_options([{x, X}|T], State) ->
    parse_options(T, State#state{x=X});
parse_options([{y, Y}|T], State) ->
    parse_options(T, State#state{y=Y});
parse_options([{orientation, Orientation}|T], State) ->
    parse_options(T, State#state{orientation=Orientation});
parse_options([{regen_rate, Reg}|T], State) ->
    parse_options(T, State#state{regen_rate=Reg});
parse_options([{hp, HP}|T], State) ->
    parse_options(T, State#state{hp=HP,max_hp=HP});
parse_options([], State) ->
    State.

async_regenerate() ->
    timer:send_after(self(), ?REGEN_INTERVAL, ?REGENERATE).
