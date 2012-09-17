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

-export([all_ids/0,
         total_players/0,
         uniq/0,
         lookup_actor/1]).

-record(state, {
}).

%%
%% External API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all_ids() ->
    ets:select(actors, ets:fun2ms(fun(#actor{id=Id}) -> Id end)).

total_players() ->
    length(ets:select(actors, ets:fun2ms(fun(#actor{id=Id,type=player}) -> ok end))).

uniq() ->
    ets:update_counter(bq_util, uniq, 1).

lookup_actor(Id) ->
    case ets:lookup(actors, Id) of
        [Actor] -> Actor;
        [] -> undefined
    end.

%%
%% gen_server callbacks
%%

init(_) ->
    ets:new(bq_util, [public, named_table]),
    ets:insert(bq_util, {uniq, 0}),
    ets:new(actors, [public, named_table, {keypos, 2}]),
    State = #state{},
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
