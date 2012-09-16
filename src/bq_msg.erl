-module(bq_msg).


-export([encode/1, decode/1, dump/1]).

-export([commands/0, command_by_id/1, command_by_name/1]).
-export([entities/0, entity_by_id/1, entity_by_name/1]).
-export([orients/0, orient_by_id/1, orient_by_name/1]).


-export([encode_atom/1]).


decode(<<"go">>) -> go;
decode(Text) when is_binary(Text) ->
    List = mochijson2:decode(Text),
    case List of
        [Cmd|Rest] when is_number(Cmd) -> parse([command_by_id(Cmd)|Rest]);
        [Cmd1|_] when is_list(Cmd1) -> [parse([command_by_id(Cmd)|Rest]) || [Cmd|Rest] <- List]
    end.


parse([hello,Name,Armor,Weapon]) -> [hello, Name, entity_by_id(Armor), entity_by_id(Weapon)];
parse([spawn,Id,1,X,Y,Name,Orient,Armor,Weapon]) -> [spawn,Id,1,X,Y,Name,orient_by_id(Orient),entity_by_id(Armor),entity_by_id(Weapon)];
parse([spawn,Id,Type,X,Y]) -> [spawn,Id,entity_by_id(Type),X,Y];
parse([spawn,Id,Type,X,Y,Orient]) -> [spawn,Id,entity_by_id(Type),X,Y,orient_by_id(Orient)];
parse([drop,EntityId,DropId,Type,Haters]) -> [drop,EntityId,DropId,entity_by_id(Type),Haters];
parse(Cmd) -> Cmd.




fmt(Format, Args) -> iolist_to_binary(io_lib:format(Format, Args)).


dump([hello,Name,Armor,Weapon]) -> fmt("#hello{name=~s,armor=~p,weapon=~p}", [Name, Armor, Weapon]);
dump([welcome,Id,Name,X,Y,HP]) -> fmt("#welcome{id=~p,name=~s,x=~p,y=~p,hp=~p}", [Id,Name,X,Y,HP]);
% server/js/player.js:252
dump([spawn,Id,1,X,Y,Name,Orient,Armor,Weapon]) -> fmt("#spawn{id=~p,type=warrior,x=~p,y=~p,name=~s,orient=~p,armor=~p,weapon=~p}", [Id,X,Y,Name,Orient,Armor,Weapon]);
dump([spawn,Id,Type,X,Y]) -> fmt("#spawn{id=~p,type=~p,x=~p,y=~p}", [Id,Type,X,Y]);
dump([spawn,Id,Type,X,Y,Orient]) -> fmt("#spawn{id=~p,type=~p,x=~p,y=~p,orient=~p}", [Id,Type,X,Y,Orient]);
dump([move,Id,X,Y]) -> fmt("#move{id=~p,x=~p,y=~p}", [Id,X,Y]);
dump([Cmd|Rest]) when is_atom(Cmd) -> fmt("#~s~240p", [Cmd,Rest]);
dump([Cmd|_] = List) when is_list(Cmd) -> [<<(dump(C))/binary, "">> || C <- List].




encode(Msg) ->
    iolist_to_binary(mochijson2:encode(encode0(Msg))).

encode0(Msg) when is_number(Msg) orelse is_binary(Msg) -> Msg;
encode0(null) -> null;
encode0(undefined) -> undefined;
encode0(Msg) when is_atom(Msg) -> encode_atom(Msg);
encode0(Msg) when is_list(Msg) -> [encode0(E) || E <- Msg].



encode_atom(Msg) when is_atom(Msg) ->
    try encode_atom0(Msg) of
        Result -> Result
    catch
        throw:{int,Value} -> Value
    end.

encode_atom0(Msg) ->
    lists:member(Msg, commands()) andalso throw({int,command_by_name(Msg)}),
    lists:member(Msg, entities()) andalso throw({int,entity_by_name(Msg)}),
    lists:member(Msg, orients()) andalso throw({int,orient_by_id(Msg)}),
    erlang:error({unknown_atom, Msg}).




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
    