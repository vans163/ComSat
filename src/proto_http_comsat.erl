-module(proto_http_comsat).

-compile(export_all).

-include("global.hrl").


response(Socket) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, http_bin}]),
    case transport_recv(Socket, 0, ?TIMEOUT) of
        {ok, {http_error, Body}} -> {http_error, Body};
        {ok, {http_response, _, StatusCode, _}} -> 
            HttpHeaders = recv_headers(Socket),
            Body = recv_body(Socket, HttpHeaders),
            {ok, StatusCode, HttpHeaders, Body};

        Wtf -> Wtf
    end
    .


recv_headers(Socket) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, httph_bin}]),
    recv_headers_1(Socket).

%TODO: This does not count the Header Key, ops
recv_headers_1(Socket) -> recv_headers_1(Socket, #{}, 0).
recv_headers_1(_, _, Size) when Size > ?HTTP_MAX_HEADER_SIZE -> throw(max_header_size_exceeded);
recv_headers_1(Socket, Map, Size) ->
    case transport_recv(Socket, 0, ?TIMEOUT) of
        {ok, http_error, Resp} -> Resp;
        {ok, {http_header,_,Key,undefined, Value}} -> 
            recv_headers_1(Socket, Map#{Key=>Value}, Size + byte_size(Value));
        {ok, http_eoh} -> Map
    end.


recv_body(Socket, HttpHeaders) when is_map(HttpHeaders) ->
    case maps:get('Transfer-Encoding', HttpHeaders, undefined) of
        undefined ->
            case maps:get('Content-Length', HttpHeaders, undefined) of
                undefined ->
                    recv_body_full(Socket);
                ContLen -> 
                    recv_body(Socket, ContLen)
            end;

        _ -> 
            throw(transfer_encoding_not_supported)
    end;
recv_body(Socket, undefined) -> <<>>;
recv_body(Socket, ContLen) when is_binary(ContLen) -> 
    recv_body(Socket, binary_to_integer(ContLen));
%recv_body(Socket, ContLen) when ContLen > ?HTTP_MAX_BODY_SIZE -> throw(max_body_size_exceeded);
recv_body(Socket, ContLen) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    {ok, Body} = transport_recv(Socket, ContLen, ?TIMEOUT),
    Body.

recv_body_full(Socket) -> recv_body_full(Socket, <<>>).
recv_body_full(Socket, Acc) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    case transport_recv(Socket, 0, ?TIMEOUT) of
        {ok, Body} ->
            recv_body_full(Socket, <<Acc/binary, Body/binary>>);
        {error, closed} -> Acc
    end
    .


request(Type, Url, Headers, Body) when is_binary(Url) -> 
    request(Type, unicode:characters_to_list(Url), Headers, Body); 
request(Type, Url, Headers, Body) ->
    case http_uri:parse(Url) of
        {error, Err} -> throw({invalid_uri, Err});

        {ok,{Scheme, _, Host, Port, Path, Query}} ->
            Head = <<Type/binary, " ", 
                (unicode:characters_to_binary(Path))/binary, 
                (unicode:characters_to_binary(Query))/binary, " HTTP/1.1\r\n">>,

            Headers2 = maps:merge(#{
                "User-Agent"=> "Mozilla/5.0 ComSat",
                "Host"=> Host, 
                "Connection"=> "close"
            }, Headers),
            Headers3 = maps:put("Content-Length", integer_to_list(byte_size(Body)), Headers2),

            HeaderBin = maps:fold(fun(K,V,Acc) ->
                    if
                        is_list(K), is_list(V) ->
                            <<Acc/binary, (unicode:characters_to_binary(K ++ ": " ++ V ++ "\r\n"))/binary>>;
                        is_binary(K), is_binary(V) ->
                            <<Acc/binary, K/binary, ": ", V/binary, "\r\n">>;
                            
                        true ->
                            throw(invalid_header_binary_OR_list)
                    end
                end,
                <<>>,
                Headers3
            ),

            Full = <<Head/binary, HeaderBin/binary, "\r\n", Body/binary>>,

            {Scheme, hostname_to_ip(Host), Full}
    end
    .