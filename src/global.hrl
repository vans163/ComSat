-define(TIMEOUT, 30000).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

-define(B(List), unicode:characters_to_binary(List)).
-define(B(AA, BB), iolist_to_binary([AA, BB])).

-define(I(Term), if 
    is_binary(Term) -> binary_to_integer(Term); 
    is_list(Term) -> list_to_integer(Term);
    true -> Term
end).

transport_setopts(SSLSocket, Opts) when is_tuple(SSLSocket), element(1, SSLSocket) =:= sslsocket -> ssl:setopts(SSLSocket, Opts);
transport_setopts(Socket, Opts) -> inet:setopts(Socket, Opts).

transport_send(SSLSocket, Payload) when is_tuple(SSLSocket), element(1, SSLSocket) =:= sslsocket -> ssl:send(SSLSocket, Payload);
transport_send(Socket, Payload) -> gen_tcp:send(Socket, Payload).

transport_recv(SSLSocket, M, T) when is_tuple(SSLSocket), element(1, SSLSocket) =:= sslsocket -> ssl:recv(SSLSocket, M, T);
transport_recv(Socket, M, T) -> gen_tcp:recv(Socket, M, T).

transport_close(SSLSocket) when is_tuple(SSLSocket), element(1, SSLSocket) =:= sslsocket -> ssl:close(SSLSocket);
transport_close(Socket) -> gen_tcp:close(Socket).

transport_peername(SSLSocket) when is_tuple(SSLSocket), element(1, SSLSocket) =:= sslsocket -> ssl:peername(SSLSocket);
transport_peername(Socket) -> inet:peername(Socket).

hostname_to_ip(Host) when is_binary(Host) -> hostname_to_ip(unicode:characters_to_list(Host));
hostname_to_ip(Host) ->
    case inet:gethostbyname(Host) of
        {ok, {hostent, _, _, inet, 4, Addrs}} -> lists:nth(rand:uniform(length(Addrs)), Addrs);
        _ -> throw(dns_lookup_failed)
    end.

normalize_binary(Term) ->
    if
        is_binary(Term) -> Term;
        is_atom(Term) -> atom_to_binary(Term, utf8);
        is_integer(Term) -> integer_to_binary(Term);
        is_list(Term) ->
            unicode:characters_to_binary(http_uri:encode(unicode:characters_to_list(Term)))
    end.

normalize_map(Map) ->
    maps:fold(fun(K,V,A) ->
        KBin = normalize_binary(K),
        VBin = normalize_binary(V),
        A#{KBin=> VBin}
    end, #{}, Map).
