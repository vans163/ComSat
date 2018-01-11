-module(comsat_socks5).
-compile(export_all).

do_server_handshake(TargetAddr, TargetPort, Socket, Username, Password, Timeout) ->
    inet:setopts(Socket, [binary, {active, false}]),
    case Username == undefined of
        true -> 
            ok = gen_tcp:send(Socket, <<5,1,0>>),
            {ok, <<5,0>>} = gen_tcp:recv(Socket, 2, Timeout);

        false -> 
            ok = gen_tcp:send(Socket, <<5,1,2>>),
            {ok, <<5,2>>} = gen_tcp:recv(Socket, 2, Timeout),
            ok = gen_tcp:send(Socket, <<1,(byte_size(Username)):8, Username/binary, (byte_size(Password)):8, Password/binary>>),
            {ok, <<1,0>>} = gen_tcp:recv(Socket, 2, Timeout)
    end,
    ok = gen_tcp:send(Socket, <<5,1,0>>),
    TargetAddrType = case inet:parse_address(unicode:characters_to_list(TargetAddr)) of
        {error, einval} -> <<3>>;
        {ok, _} -> <<1>>
    end,
    ok = gen_tcp:send(Socket, TargetAddrType),
    case TargetAddrType of
        <<1>> ->
            {ok, {IPA, IPB, IPC, IPD}} = inet:parse_address(unicode:characters_to_list(TargetAddr)),
            ok = gen_tcp:send(Socket, <<IPA, IPB, IPC, IPD>>);
        <<3>> ->
            ok = gen_tcp:send(Socket, <<(byte_size(TargetAddr)):8>>),
            ok = gen_tcp:send(Socket, TargetAddr)
    end,
    ok = gen_tcp:send(Socket, <<TargetPort:16>>),
    {ok, <<5,0,0,RAddrType/binary>>} = gen_tcp:recv(Socket, 4, Timeout),
    case RAddrType of
        <<1>> ->
            {ok, _} = gen_tcp:recv(Socket, 4, Timeout);
        <<3>> ->
            {ok, RTarLen} = gen_tcp:recv(Socket, 1, Timeout),
            {ok, _} = gen_tcp:recv(Socket, RTarLen, Timeout) 
    end,
    {ok, _} = gen_tcp:recv(Socket, 2, Timeout),
    ok.