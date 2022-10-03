-module(comsat_core_uri).
-compile(export_all).

parse(Url) ->
    Uri = uri_string:parse(Url),
    
    Scheme = maps:get(scheme, Uri, nil),
    UserInfo = maps:get(userinfo, Uri, ""),
    Host = maps:get(host, Uri, ""),
    Path = maps:get(path, Uri, "/"),
    Query = maps:get(query, Uri, ""),

    Port = case Scheme of
        <<"http">> -> 80;
        <<"https">> -> 443;
        <<"ws">> -> 80;
        <<"wss">> -> 443;
        _ -> 80
    end,
               
    Origin = io_lib:format("~s://~s:~p", [Scheme,Host,Port]),
    Origin2 = unicode:characters_to_binary(Origin),

    {Scheme, UserInfo, Origin2, Host, Path, Query, Host, Port}.
