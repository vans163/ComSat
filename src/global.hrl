-define(TIMEOUT, 30000).

-define(HTTP_MAX_HEADER_SIZE, 8000).
%-define(HTTP_MAX_BODY_SIZE, 64000).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.


unix_time() -> {A,B,_} = os:timestamp(), (A * 1000000) + B.

transport_setopts(SSLSocket={sslsocket, _, _}, Opts) -> ssl:setopts(SSLSocket, Opts);
transport_setopts(Socket, Opts) -> inet:setopts(Socket, Opts).

transport_send(SSLSocket={sslsocket, _, _}, Payload) -> ssl:send(SSLSocket, Payload);
transport_send(Socket, Payload) -> gen_tcp:send(Socket, Payload).

transport_recv(SSLSocket={sslsocket, _, _}, M, T) -> ssl:recv(SSLSocket, M, T);
transport_recv(Socket, M, T) -> gen_tcp:recv(Socket, M, T).

transport_close(SSLSocket={sslsocket, _, _}) -> ssl:close(SSLSocket);
transport_close(Socket) -> gen_tcp:close(Socket).

transport_peername(SSLSocket={sslsocket, _, _}) -> ssl:peername(SSLSocket);
transport_peername(Socket) -> inet:peername(Socket).

hostname_to_ip(Host) when is_binary(Host) -> hostname_to_ip(unicode:characters_to_list(Host));
hostname_to_ip(Host) ->
    case inet_res:gethostbyname(Host) of
        {ok, {hostent, _, _, inet, 4, Addrs}} -> hd(Addrs);
        _ -> throw(dns_lookup_failed)
    end
    .