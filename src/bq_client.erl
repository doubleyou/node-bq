-module(bq_client).

-export([init/3]).
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {
    logged_in = false,
    id
}).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! accept_client,
    {ok, Req, #state{}}.

websocket_handle({text, Msg = <<"[0,",_/binary>>}, Req, State = #state{logged_in=false}) ->
    [hello, Name, Armor, Weapon] = bq_msg:decode(Msg),
    bq_player:start_link(self(), Name, Armor, Weapon),

    lager:info("Test ~p", [gproc:lookup_value({n, l, {player, Name}})]),
    Pid = bq_player:by_name(Name),
    [Id, X, Y, HP] = gen_server:call(Pid, get),

    WelcomeMsg = [welcome, Id, Name, X, Y, HP],

    TotalChars = bq_world:total_players(),
    PopulationMsg = [population, TotalChars, TotalChars],

    ListMsg = [list | bq_world:all_ids()],

    self() ! {json, [PopulationMsg, ListMsg]},
    reply(WelcomeMsg, Req, State#state{logged_in = true, id = Id});
websocket_handle({text, Msg}, Req, State) ->
    Command = bq_msg:decode(Msg),
%%    lager:debug("user ~p> ~p", [Id,Command]),

    case bq_character:handle(Command, State) of
        {reply, Reply, NewState} ->
            reply(Reply, Req, NewState);
        {noreply, NewState} ->
            {ok, Req, NewState}
    end.

websocket_info(accept_client, Req, State) ->
    {reply, {text, <<"go">>}, Req, State};
websocket_info({json, Msg}, Req, State) ->
    reply(Msg, Req, State);
websocket_info(Msg, Req, State) ->
    reply(Msg, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
    ok.

reply(Msg, Req, State) ->
    JSON = bq_msg:encode(Msg),
    lager:debug("erlang> ~p", [Msg]),    
    {reply, {text, JSON}, Req, State}.
