-module(bq_http).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-export([onopen/1, onmessage/2, onclose/1]).

-include("bq.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    self() ! accept_client,
    % erlang:send_after(1000, self(), population),
    % erlang:send_after(1000, self(), move),
    % FIXME: link and monitor
    {ok, Upstream} = websocket_client:start_link(proplists:get_value(upstream, Opts), ?MODULE, [self()]),
    {ok, Req, #client{upstream = Upstream}}.

websocket_handle({text, Msg}, Req, #client{upstream = Upstream} = State) ->
    lager:debug("browser> ~p", [Msg]),    
    websocket_client:write(Upstream, {text, Msg}),
    
    Command = decode(Msg),
    
    case bq_controller:handle(Command, State) of
        {reply, Reply, NewState} ->
            reply(Reply, Req, NewState);
        {noreply, NewState} ->
            {ok, Req, NewState}
    end.

websocket_info(accept_client, Req, State) ->
    {reply, {text, <<"go">>}, Req, State};
% websocket_info(population, Req, State) ->
%     Pop = 1,
%     reply([17, Pop, Pop], Req, State);
% websocket_info(move, Req, State) ->
%     {Reply, NewState} = move([0, 0], State),
%     reply(Reply, Req, NewState);
websocket_info({json, Msg}, Req, State) ->
    reply(Msg, Req, State);
websocket_info({text, Text}, Req, State) ->
    lager:debug("Text from node: ~p", [Text]),
    {reply, {text, Text},Req, State};
websocket_info(Msg, Req, State) ->
    reply(Msg, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
    ok.

decode(<<"go">>) -> go;
decode(Text) -> 
    [Cmd|Rest] = mochijson2:decode(Text),
    [cmd_atom(Cmd)|Rest].

cmd_atom(N) -> 
    lists:nth(N+1, cmd_atoms()).

cmd_number(Cmd) ->
    cmd_number(Cmd, cmd_atoms(), 0).

cmd_number(Cmd, [Cmd|_], N) -> N;
cmd_number(Cmd, [_|Atoms], N) -> cmd_number(Cmd, Atoms, N+1);
cmd_number(Cmd, [], _) -> erlang:error({invalid_command, Cmd}).

cmd_atoms() ->
    [hello, welcome, spawn, despawn, move, lootmove, aggro, attack, hit,
    hurt, health, chat, loot, equip, drop, teleport, damage, population, kill, list,
    who, zone, destroy, hp, blink, open, check].



encode(Msg) when is_number(Msg) orelse is_binary(Msg) -> Msg;
encode(null) -> null;
encode(undefined) -> undefined;
encode(Msg) when is_atom(Msg) -> cmd_number(Msg);
encode(Msg) when is_list(Msg) -> [encode(E) || E <- Msg].


reply(Msg, Req, State) ->
    JSON = iolist_to_binary(mochijson2:encode(encode(Msg))),
    lager:debug("erlang> ~p", [JSON]),    
    {reply, {text, JSON}, Req, State}.



onopen([Pid]) ->
  lager:debug("Upstream connected"),
  {ok, Pid}.

onmessage({close, Code}, Pid) ->
    % Pid ! {close, Code},
    lager:debug("Closing received from node: ~p", [Code]),
    {ok, Pid};

onmessage({text, Message}, Pid) ->
  lager:debug("nodejs> ~p", [Message]),
  %%% !!!!!!!!!!!!!!!!
  %%% !!!!!!!!!!!!!!!
  %%%  If uncomment next line, all Node replies will be proxies back to browser and everything will work.
  %%%  We don't want it 
  % Pid ! {text, Message},
  {ok, Pid};

onmessage({binary, Message}, Pid) ->
  lager:debug("Bin from node: ~p", [erlang:binary_to_term(Message)]),
  Pid ! {binary, Message},
  {ok, Pid}.

onclose(Pid) ->
  {ok, Pid}.
