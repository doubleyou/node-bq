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
          {[<<"shared">>, '...'], cowboy_http_static, [
            {directory, <<"node/shared">>},
            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
          ]},
          {[<<"proxy">>, '...'], bq_proxy, [{upstream, "http://localhost:8001/"}]},
          {'_', bq_http, []}
        ]}
    ],

    cowboy:start_listener(bq_http_server, 10,
        cowboy_tcp_transport, [{port, 8000}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    lager:set_loglevel(lager_console_backend, debug),
    ok.

