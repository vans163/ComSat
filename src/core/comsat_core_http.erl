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
    transport_setopts(Socket, [{active, false}, {packet, raw}]),
    get_response_1(Socket, Timeout, <<>>).
get_response_1(Socket, Timeout, Buf) ->
    case transport_recv(Socket, 0, Timeout) of
        {ok, Bin} ->
            Buf2 = <<Buf/binary, Bin/binary>>,
            EndOfHeader = binary:match(Buf2, <<"\r\n\r\n">>),
            case EndOfHeader of
                nomatch -> get_response_1(Socket, Timeout, Buf2);
                {Pos,_} ->
                    <<HeaderBin:Pos/binary, _:32, Buf3/binary>> = Buf2,
                    [Req|Headers] = binary:split(HeaderBin, <<"\r\n">>, [global]),
                    [_,StatusCode,_] = binary:split(Req, <<" ">>, [global]),
                    StatusCodeI = erlang:binary_to_integer(StatusCode),

                    HttpHeaders = lists:foldl(fun(Line,Acc) ->
                        [K,V] = binary:split(Line, <<": ">>),
                        maps:put(K, V, Acc)
                    end, #{}, Headers),

                    %io:format("~p ~p~n",[StatusCodeI, HttpHeaders]),

                    Body = recv_body(Socket, Timeout, HttpHeaders, Buf3),

                    {ok, StatusCodeI, HttpHeaders, Body}
            end;

        F -> F 
    end.

recv_body(Socket, Timeout, #{<<"Transfer-Encoding">>:= <<"chunked">>}, BodyBuf) ->
    throw(recv_body_chunked_encoding),
    recv_body_chunked(Socket, Timeout, BodyBuf);
recv_body(Socket, Timeout, #{<<"Content-Length">>:= ContLen}, BodyBuf) ->
    recv_body_content_length(Socket, Timeout, ?I(ContLen), BodyBuf);
recv_body(_Socket, _Timeout, #{<<"Upgrade">>:= <<"websocket">>}, _) ->
    <<>>;
recv_body(Socket, Timeout, #{<<"Connection">>:= <<"close">>}, BodyBuf) ->
    recv_body_full(Socket, Timeout, BodyBuf);
recv_body(_Socket, _Timeout, _, _) -> throw(recv_body_no_clause).

recv_body_chunked(Socket, Timeout, Acc) ->
    transport_setopts(Socket, [{active, false}, {packet, line}, binary]),
    case transport_recv(Socket, 0, Timeout) of
        %TODO: Check for 'Trailer:' header
        {ok, <<"0\r\n">>} ->
            _ = transport_recv(Socket, 2, Timeout),
            Acc;

        {ok, ChunkSize} -> 
            ChunkSizeReal = binary:part(ChunkSize, 0, byte_size(ChunkSize)-2),
            ChunkSizeInt = httpd_util:hexlist_to_integer(binary_to_list(ChunkSizeReal)),
            transport_setopts(Socket, [{active, false}, {packet, raw}, binary]),
            {ok, Chunk} = transport_recv(Socket, ChunkSizeInt, Timeout),
            _ = transport_recv(Socket, 2, Timeout),
            recv_body_chunked(Socket, Timeout, <<Acc/binary, Chunk/binary>>)
    end.

recv_body_content_length(_, _, <<"0">>, _) -> <<>>;
recv_body_content_length(_, _, ContLen, BodyBuf) when ContLen - byte_size(BodyBuf) == 0 -> BodyBuf;
recv_body_content_length(Socket, Timeout, ContLen, BodyBuf) ->
    {ok, Body} = transport_recv(Socket, ContLen-byte_size(BodyBuf), Timeout),
    <<BodyBuf/binary, Body/binary>>.

recv_body_full(Socket, Timeout, BodyBuf) ->
    case transport_recv(Socket, 0, Timeout) of
        {ok, Body} ->
            recv_body_full(Socket, Timeout, <<BodyBuf/binary, Body/binary>>);
        {error, closed} -> BodyBuf
    end.


%dbg:tracer().  dbg:p(all, [call]). dbg:tpl(gen_tcp, recv, x).

