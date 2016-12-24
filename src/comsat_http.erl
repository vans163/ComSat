-module(comsat_http).
-compile(export_all).
-compile({no_auto_import,[get/1]}).
-include("global.hrl").

get(Url) -> get(Url, #{}, #{}).
get(Url, Headers, Opts) -> request(<<"GET">>, Url, Headers, <<>>, Opts).

post(Url, Body) -> post(Url, #{}, Body, #{}).
post(Url, Headers, Body, Opts) -> request(<<"POST">>, Url, Headers, Body, Opts).

request(Type, Url, ReqHeaders, ReqBody, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    Redirect = maps:get(follow_redirect, Opts, true),

    {Scheme, _, _, Host, Path, Query, DNSName, Port} = comsat_core_uri:parse(Url),

    Ip = hostname_to_ip(DNSName),
    case Scheme of
        %scheme http or https
        R1 when (R1 =:= <<"http">>) or (R1 =:= <<"https">>) ->
            RequestBin = comsat_core_http:build_request(Type, Path, Query, Host, ReqHeaders, ReqBody),
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active, false}, binary], Timeout),
            ok = transport_send(Socket, RequestBin),
            {ok, StatusCode, Headers, ReplyBody} = comsat_core_http:get_response(Socket, Timeout),
            case Redirect of
                true when (StatusCode =:= 301) or (StatusCode =:= 302) ->
                    get(maps:get('Location', Headers), ReqHeaders, Opts);

                _ -> {ok, StatusCode, Headers, ReplyBody}
            end
    end.

ws_connect(Url) -> ws_connect(Url, #{}, #{}).
ws_connect(Url, ReqHeaders, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),

    {Scheme, _, Origin, Host, Path, Query, DNSName, Port} = comsat_core_uri:parse(Url),
    Ip = hostname_to_ip(DNSName),
    true = ((Scheme == <<"ws">>) or (Scheme == <<"wss">>)),

    Key = base64:encode(crypto:strong_rand_bytes(16)),
    %Secret = base64:encode(crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),

    RequestBin = comsat_core_http:build_request(<<"GET">>, Path, Query, Host, maps:merge(#{
        "Connection"=> "Upgrade",
        "Upgrade"=> "websocket",
        "Origin"=> Origin,
        "Pragma"=> "no-cache",
        "Cache-Control"=> "no-cache",
        "Sec-WebSocket-Version"=> "13",
        "Sec-WebSocket-Key"=> Key
    }, ReqHeaders), <<>>),

    {ok, Socket} = gen_tcp:connect(Ip, Port, [{active, false}, binary], Timeout),
    ok = transport_send(Socket, RequestBin),

    {ok, 101, _ReplyHeaders, _ReplyBody} = comsat_core_http:get_response(Socket, Timeout),
    Socket.