-module(bq_mob_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([add_mob/1]).

%%
%% External API
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_mob(Options) ->
    supervisor:start_child(?MODULE, Options).

%%
%% supervisor callbacks
%%

init([]) ->
    Child = {none, {bq_mob, start_link, []}, transient, brutal_kill, worker, [bq_mob]},
    {ok, { {simple_one_for_one, 5, 10}, [Child]} }.
