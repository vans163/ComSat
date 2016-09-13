-module(comsat).

-compile(export_all).
%-export([]).

-compile({no_auto_import,[get/1]}).
-include("global.hrl").

get(Url) ->
    Socket = case proto_http_comsat:request(<<"GET">>, Url, #{}, <<"">>) of
        {http, Ip, Bin} ->
            {ok, Sock} = gen_tcp:connect(Ip, 80, [binary], ?TIMEOUT),
            Sock;

        {https, Ip, Bin} -> 
            {ok, SSLSock} = ssl:connect(Ip, 443, [binary], ?TIMEOUT),
            SSLSock
    end,

    ok = transport_send(Socket, Bin),
    {ok, StatusCode, Headers, Body} = proto_http_comsat:response(Socket),
    case StatusCode of
        301 -> get(maps:get('Location', Headers));
        302 -> get(maps:get('Location', Headers));
        _ -> 
            {ok, StatusCode, Headers, Body},
            Body
    end
    .

get(Url, Opts) -> pass
    .

post() -> pass
    .


get2() -> http2_not_implemented.
post2()-> http2_not_implemented.