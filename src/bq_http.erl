-module(bq_http).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).


-include("bq.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! accept_client,
    % erlang:send_after(1000, self(), population),
    % erlang:send_after(1000, self(), move),
    % FIXME: link and monitor
    timer:send_interval(2000, regenerate),
    {ok, Req, #client{hitpoints = 100}}.

websocket_handle({text, Msg}, Req, #client{} = State) ->
    Command = bq_msg:decode(Msg),
    lager:debug("browser> ~p", [Command]),

    case bq_controller:handle(Command, State) of
        {reply, Reply, NewState} ->
            reply(Reply, Req, NewState);
        {noreply, NewState} ->
            {ok, Req, NewState}
    end.
    % {ok, Req, State}.

websocket_info(regenerate, Req, State) ->
    case bq_controller:regenerate(State) of
        {reply, Reply, NewState} ->
            reply(Reply, Req, NewState);
        {noreply, NewState} ->
            {ok, Req, NewState}
    end;
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



reply(Msg, Req, State) ->
    JSON = bq_msg:encode(Msg),
    lager:debug("erlang> ~p", [Msg]),    
    {reply, {text, JSON}, Req, State}.



