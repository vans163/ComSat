-module(comsat_core_http).
-compile(export_all).
-include("../global.hrl").

build_request(Type, Path, Query, Host) ->
    build_request(Type, Path, Query, Host, #{}, <<>>).
build_request(Type, Path, Query, Host, Headers, Body) ->
    Head = <<Type/binary, " ", 
        (unicode:characters_to_binary(Path))/binary, 
        (unicode:characters_to_binary(Query))/binary, " HTTP/1.1\r\n">>,

    Headers2 = maps:merge(#{
        <<"User-Agent">>=> <<"Mozilla/5.0 ComSat">>,
        <<"Host">>=> Host, 
        <<"Connection">>=> <<"close">>
    }, Headers),
    Headers3 = maps:put(<<"Content-Length">>, integer_to_binary(byte_size(Body)), Headers2),

    HeaderBin = maps:fold(fun(K,V,Acc) ->
            if
                is_list(K), is_binary(V) ->
                    KB1 = unicode:characters_to_binary(K),
                    <<Acc/binary, KB1/binary, ": ", V/binary,"\r\n">>;
                is_binary(K), is_list(V) ->
                    VB1 = unicode:characters_to_binary(V),
                    <<Acc/binary, K/binary, ": ", VB1/binary,"\r\n">>;

                is_list(K), is_list(V) ->
                    <<Acc/binary, (unicode:characters_to_binary(K ++ ": " ++ V ++ "\r\n"))/binary>>;
                is_binary(K), is_binary(V) ->
                    <<Acc/binary, K/binary, ": ", V/binary, "\r\n">>
            end
        end, <<>>, Headers3
    ),

    <<Head/binary, HeaderBin/binary, "\r\n", Body/binary>>.



get_response(Socket, Timeout) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, http_bin}]),
    case transport_recv(Socket, 0, Timeout) of
        {ok, {http_error, Body}} -> {http_error, Body};
        {ok, {http_response, _, StatusCode, _}} -> 
            HttpHeaders = recv_headers(Socket, Timeout),
            Body = recv_body(Socket, Timeout, HttpHeaders),
            {ok, StatusCode, HttpHeaders, Body};

        F -> F 
    end.

recv_headers(Socket, Timeout) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, httph_bin}]),
    recv_headers_1(Socket, Timeout).

recv_headers_1(Socket, Timeout) -> recv_headers_1(Socket, Timeout, #{}, 0).
recv_headers_1(_, _, _, Size) when Size > ?HTTP_MAX_HEADER_SIZE -> throw(max_header_size_exceeded);
recv_headers_1(Socket, Timeout, Map, Size) ->
    case transport_recv(Socket, 0, Timeout) of
        {ok, http_error, Resp} -> {httph_error, Resp};
        {ok, {http_header, _, Key, undefined, Value}} -> 
            KeyBin = case is_atom(Key) of true -> atom_to_binary(Key, latin1); false -> Key end,
            recv_headers_1(Socket, Timeout, Map#{Key=>Value}, Size + byte_size(KeyBin) + byte_size(Value));
        {ok, http_eoh} -> Map
    end.


recv_body(Socket, Timeout, #{'Transfer-Encoding':= <<"chunked">>}) ->
    recv_body_chunked(Socket, Timeout);
recv_body(Socket, Timeout, #{'Content-Length':= ContLen}) ->
    recv_body_content_length(Socket, Timeout, ContLen);
recv_body(Socket, Timeout, #{'Upgrade':= <<"websocket">>}) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    <<>>;
recv_body(Socket, Timeout, _) ->
    recv_body_full(Socket, Timeout).

recv_body_chunked(Socket, Timeout) -> recv_body_chunked(Socket, Timeout, <<>>).
recv_body_chunked(Socket, Timeout, Acc) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, line}, binary]),
    case transport_recv(Socket, 0, Timeout) of
        %TODO: Check for 'Trailer:' header
        {ok, <<"0\r\n">>} ->
            _ = transport_recv(Socket, 2, Timeout),
            Acc;

        {ok, ChunkSize} -> 
            ChunkSizeReal = binary:part(ChunkSize, 0, byte_size(ChunkSize)-2),
            ChunkSizeInt = httpd_util:hexlist_to_integer(binary_to_list(ChunkSizeReal)),
            ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
            {ok, Chunk} = transport_recv(Socket, ChunkSizeInt, Timeout),
            _ = transport_recv(Socket, 2, Timeout),
            recv_body_chunked(Socket, <<Acc/binary, Chunk/binary>>)
    end.

recv_body_content_length(Socket, Timeout, ContLen) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    {ok, Body} = transport_recv(Socket, ?I(ContLen), Timeout),
    Body.

recv_body_full(Socket, Timeout) -> recv_body_full(Socket, Timeout, <<>>).
recv_body_full(Socket, Timeout, Acc) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    case transport_recv(Socket, 0, Timeout) of
        {ok, Body} ->
            recv_body_full(Socket, Timeout, <<Acc/binary, Body/binary>>);
        {error, closed} -> Acc
    end.


%dbg:tracer().  dbg:p(all, [call]). dbg:tpl(gen_tcp, recv, x).

