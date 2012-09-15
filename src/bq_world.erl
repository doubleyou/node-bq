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


-record(state, {
    characters = []
}).

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
    {ok, #state{}}.

handle_call({add_character, Char}, _From, State = #state{characters=Chars}) ->
    {reply, {ok, {16,233, 100}}, State#state{characters=[Char|Chars]}};
handle_call(list_id, _From, State = #state{}) ->
    {reply, [19,926,927,928,929,1020,1021,1022], State};
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
