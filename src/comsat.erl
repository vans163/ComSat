-module(comsat).

-compile(export_all).
%-export([]).

-compile({no_auto_import,[get/1]}).
-include("global.hrl").

get(Url) ->
    case proto_http_comsat:request(<<"GET">>, Url, #{}, <<"">>) of
        {http, Ip, Bin} ->
            {ok, Socket} = gen_tcp:connect(Ip, 80, [binary], ?TIMEOUT),
            ok = gen_tcp:send(Socket, Bin),
            {ok, StatusCode, Headers, Body} = proto_http_comsat:response(Socket),
            case StatusCode of
                302 -> get(maps:get('Location', Headers));
                _ -> 
                    {ok, StatusCode, Headers, Body},
                    Body
            end;

        {https, Ip, Bin} -> 
            {ok, Socket} = ssl:connect(Ip, 443, [binary], ?TIMEOUT),
            ok = ssl:send(Socket, Bin),
            {ok, StatusCode, Headers, Body} = proto_http_comsat:response(Socket),
            case StatusCode of
                302 -> get(maps:get('Location', Headers));
                _ -> 
                    {ok, StatusCode, Headers, Body},
                    Body
            end
    end
    .

get(Url, Opts) -> pass
    .

post() -> pass
    .


get2() -> http2_not_implemented.
post2()-> http2_not_implemented.