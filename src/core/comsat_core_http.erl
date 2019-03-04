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
        <<Acc/binary, K/binary, ": ", V/binary,"\r\n">>
    end, <<>>, Headers3),

    <<Head/binary, HeaderBin/binary, "\r\n", Body/binary>>.

get_response(Socket, Timeout) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, http_bin}]),
    case transport_recv(Socket, 0, Timeout) of
        {ok, {http_error, Body}} -> {http_error, Body};
        {ok, {http_response, _, StatusCode, _}} -> 
            HttpHeaders2 = recv_headers(Socket, Timeout),
            HttpHeaders = maps:fold(fun(K,V,A) ->
                K2 = if 
                    is_atom(K) -> atom_to_binary(K, utf8);
                    true -> K
                end,
                A#{K2=> V}
            end, #{}, HttpHeaders2),

            %io:format("~p~n", [HttpHeaders]),
            Body = recv_body(Socket, Timeout, HttpHeaders),
            {ok, StatusCode, HttpHeaders, Body};

        F -> F 
    end.

recv_headers(Socket, Timeout) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, httph_bin}]),
    recv_headers_1(Socket, Timeout).

recv_headers_1(Socket, Timeout) -> recv_headers_1(Socket, Timeout, #{}).
recv_headers_1(Socket, Timeout, Map) ->
    case transport_recv(Socket, 0, Timeout) of
        {ok, http_error, Resp} -> {httph_error, Resp};
        {ok, {http_header, _, Key, undefined, Value}} ->
            case maps:get(Key, Map, undefined) of
                undefined -> recv_headers_1(Socket, Timeout, Map#{Key=> Value});
                OldValue when is_list(OldValue) =:= false ->
                    recv_headers_1(Socket, Timeout, Map#{Key=> [OldValue, Value]});
                OldValue when is_list(OldValue) =:= true ->
                    recv_headers_1(Socket, Timeout, Map#{Key=> OldValue ++ [Value]})
            end;
        {ok, http_eoh} -> Map
    end.


recv_body(Socket, Timeout, #{<<"Transfer-Encoding">>:= <<"chunked">>}) ->
    recv_body_chunked(Socket, Timeout);
recv_body(Socket, Timeout, #{<<"Content-Length">>:= ContLen}) ->
    recv_body_content_length(Socket, Timeout, ContLen);
recv_body(Socket, _Timeout, #{<<"Upgrade">>:= <<"websocket">>}) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    <<>>;
recv_body(Socket, Timeout, #{<<"Connection">>:= <<"close">>}) ->
    recv_body_full(Socket, Timeout);
recv_body(_Socket, _Timeout, _) -> <<>>.

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
            recv_body_chunked(Socket, Timeout, <<Acc/binary, Chunk/binary>>)
    end.

recv_body_content_length(_, _, <<"0">>) -> <<>>;
recv_body_content_length(Socket, Timeout, ContLen) ->
    ok = transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    {ok, Body} = transport_recv(Socket, ?I(ContLen), Timeout),
    Body.

recv_body_full(Socket, Timeout) -> recv_body_full(Socket, Timeout, <<>>).
recv_body_full(Socket, Timeout, Acc) ->
    transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
    case transport_recv(Socket, 0, Timeout) of
        {ok, Body} ->
            recv_body_full(Socket, Timeout, <<Acc/binary, Body/binary>>);
        {error, closed} -> Acc
    end.


%dbg:tracer().  dbg:p(all, [call]). dbg:tpl(gen_tcp, recv, x).

