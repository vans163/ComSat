-module(comsat_http).
-compile(export_all).
-compile({no_auto_import,[get/1]}).
-include("global.hrl").

%comsat_http:get(<<"http://yahoo.com">>).

%{ok, V5} = comsat_http:get(<<"https://www.yahoo.com">>, #{}, #{keep_alive=> true}).
%{ok, _} = comsat_http:get(<<"https://www.yahoo.com">>, #{}, #{keep_alive=> true, reuse_socket=> maps:get(socket, V5)}).

get(Url) -> get(Url, #{}, #{}).
get(Url, Headers) -> get(Url, Headers, #{}).
get(Url, Headers, Opts) -> request(<<"GET">>, Url, Headers, <<>>, Opts).

post(Url, Body) -> post(Url, #{}, Body, #{}).
post(Url, Headers, Body) -> post(Url, Headers, Body, #{}).
post(Url, Headers, Body, Opts) -> request(<<"POST">>, Url, Headers, Body, Opts).

request(Type, Url, ReqHeaders2, ReqBody, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    %FollowRedirect = maps:get(follow_redirect, Opts, true),
    INetOptions = maps:get(inet_options, Opts, []),
    SSLOptions = maps:get(ssl_options, Opts, []),
    ReuseSocket = maps:get(reuse_socket, Opts, undefined),
    KeepAlive = maps:get(keep_alive, Opts, false),
    Proxy = maps:get(proxy, Opts, #{}),
    ProxyType = maps:get(type, Proxy, undefined),

    ReqHeaders = case KeepAlive of
        true -> ensure_headers_connection(ReqHeaders2, <<"keep-alive">>);
        false -> ensure_headers_connection(ReqHeaders2, <<"close">>)
    end,

    {Scheme, _, _, Host, _Path, _Query, DNSName, Port} = comsat_core_uri:parse(Url),
    %scheme http or https
    true = (Scheme =:= <<"http">>) or (Scheme =:= <<"https">>),

    Ip = hostname_to_ip(DNSName),

    case ReuseSocket of
        undefined ->
            {ok, SocketFinal, ReqHeaders3} = case ProxyType of
                undefined -> 
                    case Scheme of
                        <<"http">> ->
                            {ok, Socket} = gen_tcp:connect(Ip, Port, INetOptions++[{active, false}, binary], Timeout),
                            {ok, Socket, ReqHeaders};

                        <<"https">> ->
                            {ok, Socket} = ssl:connect(Ip, Port, INetOptions++SSLOptions++[{active, false}, binary], Timeout),
                            {ok, Socket, ReqHeaders}
                    end;

                socks5 ->
                    {ok, Socks5Socket} = gen_tcp:connect(
                        unicode:characters_to_list(maps:get(host, Proxy)),
                        maps:get(port, Proxy),
                        INetOptions++[{active, false}, binary],
                        Timeout
                    ),
                    ok = comsat_socks5:do_server_handshake(Host, Port, Socks5Socket, Timeout),
                    case Scheme of
                        <<"http">> -> {ok, Socks5Socket, ReqHeaders};
                        <<"https">> -> 
                            {ok, SSLSocks5Socket} = ssl:connect(Socks5Socket, SSLOptions, Timeout),
                            {ok, SSLSocks5Socket, ReqHeaders}
                    end;

                http ->
                    {PScheme, _, _, _PHost, _PPath, _PQuery, PDNSName, PPort} 
                        = comsat_core_uri:parse(maps:get(host, Proxy)),
                    true = (PScheme =:= <<"http">>),
                    PIp = hostname_to_ip(PDNSName),

                    {ok, ProxySocket} = gen_tcp:connect(
                        PIp,
                        PPort,
                        INetOptions++[{active, false}, binary],
                        Timeout
                    ),

                    ProxyReqHeaders2 = case KeepAlive of
                        true -> maps:merge(ReqHeaders, #{<<"Proxy-Connection">>=> <<"keep-alive">>});
                        false -> maps:merge(ReqHeaders, #{<<"Proxy-Connection">>=> <<"close">>})
                    end,

                    ProxyUsername = unicode:characters_to_binary(maps:get(username, Proxy, undefined)),
                    ProxyPassword = unicode:characters_to_binary(maps:get(password, Proxy, undefined)),
                    ProxyReqHeaders = case (ProxyUsername =/= undefined) and (ProxyPassword =/= undefined) of
                        true -> 
                            Base64 = base64:encode(<<ProxyUsername/binary,":",ProxyPassword/binary>>),
                            ProxyAuth = <<"Basic ", Base64/binary>>,
                            maps:merge(ProxyReqHeaders2, #{<<"Proxy-Authorization">>=> ProxyAuth});
                        false -> ProxyReqHeaders2
                    end,
                    case Scheme of
                        <<"http">> -> {ok, ProxySocket, ProxyReqHeaders};
                        <<"https">> ->
                            HostPort = <<Host/binary,":",(integer_to_binary(Port))/binary>>,
                            ProxyAuth2 = maps:get(<<"Proxy-Authorization">>, ProxyReqHeaders),
                            PConn = case KeepAlive of true-> <<"keep-alive">>; false-> <<"close">> end,
                            ProxyRequestBin = <<
                                "CONNECT ", HostPort/binary, " HTTP/1.1\r\n",
                                "Host: ", HostPort/binary, "\r\n",
                                "Proxy-Authorization: ", ProxyAuth2/binary, "\r\n",
                                "Proxy-Connection: ", PConn/binary, "\r\n\r\n"
                            >>,
                            %io:format("~p~n",[ProxyRequestBin]),
                            ok = gen_tcp:send(ProxySocket, ProxyRequestBin),
                            {ok, 200, _Headers, _ReplyBody} 
                                = comsat_core_http:get_response(ProxySocket, Timeout),

                            {ok, SSLSocket} = ssl:connect(ProxySocket, SSLOptions, Timeout),
                            {ok, SSLSocket, ReqHeaders}
                            %ok = comsat_proxy_http:do_server_handshake(Host, Port, ProxySocket, Timeout),                    
                    end
            end,
            request_1(SocketFinal, Type, Url, ReqHeaders3, ReqBody, Opts);

        ReuseSocket ->
            request_1(ReuseSocket, Type, Url, ReqHeaders, ReqBody, Opts)
    end.

request_1(Socket, Type, Url, ReqHeaders, ReqBody, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    FollowRedirect = maps:get(follow_redirect, Opts, true),
    KeepAlive = maps:get(keep_alive, Opts, false),

    {Scheme, _, _, Host, Path, Query, _DNSName, Port} = comsat_core_uri:parse(Url),
    RequestBin = case maps:get(<<"Proxy-Authorization">>, ReqHeaders, undefined) of
        R when (R =/= undefined) and (Scheme =:= <<"http">>) -> 
            %Little hack
            ReqHeaders2 = maps:put(<<"Connection">>, <<"close">>, ReqHeaders),
            comsat_core_http:build_request(Type, 
                <<Scheme/binary, "://", Host/binary, Path/binary>>, Query, Host, ReqHeaders2, ReqBody);
        _ -> 
            comsat_core_http:build_request(Type, Path, Query, Host, ReqHeaders, ReqBody)
    end,

    ok = transport_send(Socket, RequestBin),
    {ok, StatusCode, Headers, ReplyBody} = comsat_core_http:get_response(Socket, Timeout),
    ReplyConnection2 = maps:get('Connection', Headers, <<"close">>),
    ReplyConnection = unicode:characters_to_binary(
        string:to_lower(unicode:characters_to_list(ReplyConnection2))),

    case StatusCode of
        SC when FollowRedirect, ((SC =:= 301) or (SC =:= 302)) ->
            Location = maps:get('Location', Headers),
            {HostScheme, _, _, HostLocation, _HostPath, _HostQuery, _HostDNSName, HostPort}
                = comsat_core_uri:parse(Location),
            case (Scheme =:= HostScheme) and (Host =:= HostLocation) and (Port =:= HostPort) of
                true when ReplyConnection =:= <<"keep-alive">> -> 
                    request(Type, Location, ReqHeaders, ReqBody, Opts);
                _ ->
                    ok = transport_close(Socket),
                    request(Type, Location, ReqHeaders, ReqBody, maps:put(reuse_socket, undefined, Opts))
            end;

        _ ->
            case KeepAlive of
                true when ReplyConnection =:= <<"keep-alive">> -> 
                    {ok, #{socket=> Socket, status_code=> StatusCode, headers=> Headers, body=> ReplyBody}};

                _ -> 
                    ok = transport_close(Socket),
                    {ok, #{status_code=> StatusCode, headers=> Headers, body=> ReplyBody}}
            end
    end.

keep_alive_close(Socket) -> transport_close(Socket).

ensure_headers_connection(Headers, Value) ->
    Headers2 = maps:without([
            "Connection", <<"Connection">>,
            'Connection', 'connection'
        ], Headers),
    maps:put(<<"Connection">>, Value, Headers2).

to_query(PropList) when is_list(PropList) -> to_query(maps:from_list(PropList));
to_query(Map) ->
    Res = maps:fold(fun(K,V,A) ->
        KBin = if
            is_atom(K) -> atom_to_binary(K, utf8);
            is_binary(K); is_list(K) ->
                unicode:characters_to_binary(http_uri:encode(unicode:characters_to_list(K)))
        end,
        VBin = if 
            is_atom(V) -> atom_to_binary(V, utf8);
            is_binary(V); is_list(V) ->
                unicode:characters_to_binary(http_uri:encode(unicode:characters_to_list(V)))
        end,
        <<A/binary, KBin/binary,"=",VBin/binary,"&">>
    end, <<>>, Map),
    erlang:binary_part(Res, 0, byte_size(Res)-1).

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

    Transport = if Scheme =:= <<"wss">> -> ssl; true-> gen_tcp end,
    {ok, Socket} = Transport:connect(Ip, Port, [{active, false}, binary], Timeout),
    
    ok = transport_send(Socket, RequestBin),

    {ok, 101, _ReplyHeaders, _ReplyBody} = comsat_core_http:get_response(Socket, Timeout),
    Socket.