-module(bq_http).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-export([onopen/1, onmessage/2, onclose/1]).

-export([hello/2,
         move/2,
         chat/2]).

-record(state, {
    upstream,
    logged_in = false,
    armor = 21,
    weapon = 60,
    id = 1000,
    x = 0,
    y = 0,
    orientation = random:uniform(4)
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    self() ! accept_client,
    erlang:send_after(1000, self(), population),
    erlang:send_after(1000, self(), move),
    % FIXME: link and monitor
    {ok, Upstream} = websocket_client:start_link(proplists:get_value(upstream, Opts), ?MODULE, [self()]),
    {ok, Req, #state{upstream = Upstream}}.

websocket_handle({text, Msg}, Req, #state{upstream = Upstream} = State) ->
    websocket_client:write(Upstream, {text, Msg}),
    lager:debug("Text from browser: ~p", [Msg]),
    
    case Msg of
        <<"go">> ->    
            {ok, Req, State};
        _ ->            
            [Cmd | Rest] = mochijson2:decode(Msg),
            CmdAtom = cmd_atom(Cmd),
            % FIXME: here is failing something
            % {Reply, NewState} = case erlang:function_exported(?MODULE, CmdAtom, 2) of
            %     true -> ?MODULE:CmdAtom(Rest, State);
            %     false -> {undefined, State}
            % end,
            {Reply, NewState} = {undefined, State},
            lager:info("Cmd: ~p(~w) -> ~w~n", [CmdAtom, Rest, Reply]),
            % {reply, {text, mochijson2:encode(Reply)}, Req, NewState}.
            {ok, Req, NewState}
    end.

websocket_info(accept_client, Req, State) ->
    {reply, {text, <<"go">>}, Req, State};
websocket_info(population, Req, State) ->
    Pop = 1,
    reply([17, Pop, Pop], Req, State);
websocket_info(move, Req, State) ->
    {Reply, NewState} = move([0, 0], State),
    reply(Reply, Req, NewState);
websocket_info({text, Text}, Req, State) ->
    lager:debug("Text from node: ~p", [Text]),
    {reply, {text, Text},Req, State};
websocket_info(Msg, Req, State) ->
    reply(Msg, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
    ok.

hello([Name | _], State) ->
    ID = erlang:phash2(Name, 100000),
    NewState = State#state{
        id = ID,
        logged_in = true
    },
    lager:info("Client with id ~p connected", [ID]),
    gproc:reg({n, l, ID}),
    bq_world:add_character(ID),
    {[1, NewState#state.id, Name, 0, 0, 100], NewState}.

move([X, Y], State) ->
    {[4, State#state.id, X, Y], State#state{x = X, y = Y}}.

chat(Cmd, State) ->
    lager:info("Chat message: ~p", [Cmd]),
    {[], State}.

cmd_atom(N) -> 
    lists:nth(N+1, [hello, welcome, spawn, despawn, move, lootmove, aggro, attack, hit,
    hurt, health, 11, loot, equip, drop, teleport, damage, population, kill, list,
    who, zone, destroy, hp, blink, open, check]).


reply(Msg, Req, State) ->
    {reply, {text, mochijson2:encode(Msg)}, Req, State}.



onopen([Pid]) ->
  lager:debug("Upstream connected"),
  {ok, Pid}.

onmessage({close, Code}, Pid) ->
    % Pid ! {close, Code},
    lager:debug("Closing received from node: ~p", [Code]),
    {ok, Pid};

onmessage({text, Message}, Pid) ->
  % lager:debug("Text from node: ~s", [Message]),
  Pid ! {text, Message},
  {ok, Pid};

onmessage({binary, Message}, Pid) ->
  lager:debug("Bin from node: ~p", [erlang:binary_to_term(Message)]),
  Pid ! {binary, Message},
  {ok, Pid}.

onclose(Pid) ->
  {ok, Pid}.
