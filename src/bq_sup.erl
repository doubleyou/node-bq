-module(bq_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(Name, Role),
    {Name, {Name, start_link, []}, permanent, 5000, Role, [Name]}).

%%
%% External API
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% supervisor callbacks
%%

init([]) ->
    World = ?CHILD(bq_world, worker),
    {ok, { {one_for_one, 5, 10}, [World]} }.
