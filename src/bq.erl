-module(bq).

-export([start/0]).

start() ->
    lager:start(),
    application:start(gproc),
    application:start(cowboy),
    application:start(bq),

    Dispatch = [
        {'_', [{'_', bq_http, []}]}
    ],

    cowboy:start_listener(bq_http_server, 10,
        cowboy_tcp_transport, [{port, 8000}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).
