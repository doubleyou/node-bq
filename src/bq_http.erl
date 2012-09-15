-module(bq_http).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-export([onopen/1, onmessage/2, onclose/1]).

-export([commands/0, command_by_id/1, command_by_name/1]).
-export([entities/0, entity_by_id/1, entity_by_name/1]).
-export([orients/0, orient_by_id/1, orient_by_name/1]).

-export([encode/1, decode/1]).

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
    Command = decode(Msg),
    lager:debug("browser> ~p", [Command]),

    websocket_client:write(Upstream, {text, Msg}),

    case bq_controller:handle(Command, State) of
        {reply, Reply, NewState} ->
            reply(Reply, Req, NewState);
        {noreply, NewState} ->
            {ok, Req, NewState}
    end.
    % {ok, Req, State}.

websocket_info(accept_client, Req, State) ->
    {reply, {text, <<"go">>}, Req, State};
websocket_info({json, Msg}, Req, State) ->
    reply(Msg, Req, State);
websocket_info({text, Text}, Req, State) ->
    % lager:debug("Text from node: ~p", [Text]),
    {reply, {text, Text},Req, State};
websocket_info(Msg, Req, State) ->
    reply(Msg, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
    ok.

decode(<<"go">>) -> go;
decode(Text) when is_binary(Text) ->
    List = mochijson2:decode(Text),
    case List of
        [Cmd|Rest] when is_number(Cmd) -> [command_by_id(Cmd)|Rest];
        [Cmd1|_] when is_list(Cmd1) -> [[command_by_id(Cmd)|Rest] || [Cmd|Rest] <- List]
    end.


fmt(Format, Args) -> iolist_to_binary(io_lib:format(Format, Args)).


dump([hello,Name,Armor,Weapon]) -> fmt("#hello{name=~s,armor=~p,weapon=~p}", [Name, entity_by_id(Armor), entity_by_id(Weapon)]);
dump([welcome,Id,Name,X,Y,HP]) -> fmt("#welcome{id=~p,name=~s,x=~p,y=~p,hp=~p}", [Id,Name,X,Y,HP]);
% server/js/player.js:252
dump([spawn,Id,1,X,Y,Name,Orient,Armor,Weapon]) -> fmt("#spawn{id=~p,type=warrior,x=~p,y=~p,name=~s,orient=~p,armor=~p,weapon=~p}", [Id,X,Y,Name,orient_by_id(Orient),Armor,Weapon]);
dump([spawn,Id,Type,X,Y]) -> fmt("#spawn{id=~p,type=~p,x=~p,y=~p}", [Id,entity_by_id(Type),X,Y]);
dump([spawn,Id,Type,X,Y,Orient]) -> fmt("#spawn{id=~p,type=~p,x=~p,y=~p,orient=~p}", [Id,entity_by_id(Type),X,Y, orient_by_id(Orient)]);
dump([move,Id,X,Y]) -> fmt("#move{id=~p,x=~p,y=~p}", [Id,X,Y]);
dump([Cmd|Rest]) when is_atom(Cmd) -> fmt("#~s~240p", [Cmd,Rest]);
dump([Cmd|_] = List) when is_list(Cmd) -> [<<(dump(C))/binary, "">> || C <- List].



command_by_id(N) -> lists:nth(N+1, commands()).
command_by_name(Cmd) -> find_in_list(Cmd, commands()).

entity_by_id(N) -> lists:nth(N+1, entities()).
entity_by_name(Cmd) -> find_in_list(Cmd, entities()).

orient_by_id(N) -> lists:nth(N+1, orients()).
orient_by_name(Cmd) -> find_in_list(Cmd, orients()).

find_in_list(Atom, List) ->
    proplists:get_value(Atom, lists:zip(List, lists:seq(0,length(List)-1))).

commands() ->
    [hello, welcome, spawn, despawn, move, lootmove, aggro, attack, hit,
    hurt, health, chat, loot, equip, drop, teleport, damage, population, kill, list,
    who, zone, destroy, hp, blink, open, check].


entities() ->
    [none,warrior, rat, skeleton, goblin, ogre, spectre, crab, bat, wizard, eye, snake, skeleton2, boss, deathknight,
    none,none,none,none,none,
    firfox, clotharmor, leatherarmor, mailarmor, platearmor, readarmor, goldenarmor,
    none,none,none,none,none,none,none,none,
    flask, burger, chest, firepotion,cake,
    guard,kind,octocat,villagegirl,villager,priest,scientist,agent,rick,nyan,sorcerer,beachnpc,forestnpc,desertnpc,lavanpc,coder,
    none,none,none,none,
    sword1,sword2,redsword,goldensword,morningstar,axe,bluesword].


orients() ->
    [none,up,down,left,right].


encode(Msg) when is_number(Msg) orelse is_binary(Msg) -> Msg;
encode(null) -> null;
encode(undefined) -> undefined;
encode(Msg) when is_atom(Msg) -> command_by_name(Msg);
encode(Msg) when is_list(Msg) -> [encode(E) || E <- Msg].


reply(Msg, Req, State) ->
    JSON = iolist_to_binary(mochijson2:encode(encode(Msg))),
    lager:debug("erlang> ~p", [Msg]),    
    {reply, {text, JSON}, Req, State}.



onopen([Pid]) ->
  lager:debug("Upstream connected"),
  {ok, Pid}.

onmessage({close, Code}, Pid) ->
    % Pid ! {close, Code},
    lager:debug("Closing received from node: ~p", [Code]),
    {ok, Pid};

onmessage({text, <<"go">> = Message}, Pid) ->
    lager:debug("nodejs> ~p", [Message]),
    {ok, Pid};

onmessage({text, <<"timeout">> = Message}, Pid) ->
    lager:debug("nodejs> ~p", [Message]),
    {ok, Pid};

onmessage({text, Message}, Pid) ->
    Command = decode(Message),
    lager:debug("nodejs> ~p", [dump(Command)]),
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
