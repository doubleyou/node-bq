-module(bq).

-export([start/0]).

start() ->
    lager:start(),
    application:start(gproc),
    application:start(cowboy),
    application:start(bq),
    application:start(mimetypes),

    Dispatch = [
        {'_', [
          {[<<"client">>, '...'], cowboy_http_static, [
            {directory, <<"node/client">>},
            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
          ]},
          {'_', bq_http, []}
        ]}
    ],

    cowboy:start_listener(bq_http_server, 10,
        cowboy_tcp_transport, [{port, 8000}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).
