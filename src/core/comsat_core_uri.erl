-module(comsat_core_uri).
-compile(export_all).

parse(Url) ->
    {ok, {Scheme, Auth, Host, Port, Path, Query}} = http_uri:parse(Url, 
        [
            {scheme_defaults, [{http,80},{https,443},{ws,80},{wss,443}]}
        ]),
    Scheme2 = erlang:atom_to_binary(Scheme, unicode),
    
    Auth2 = unicode:characters_to_binary(Auth),

    Origin = io_lib:format("~p://~s:~p", [Scheme,Host,Port]),
    Origin2 = unicode:characters_to_binary(Origin),

    Host2 = unicode:characters_to_binary(Host),

    Path2 = unicode:characters_to_binary(Path),

    Query2 = unicode:characters_to_binary(Query),

    %{ok,{https,[],"www.google.com",443,"/test","?te=4"}}

    %{match, _Matches} = re:run(Url, 
    %    "^(([^:\\/?#]+):)?(\\/\\/([^\/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?", 
    %    [{capture, all_but_first, binary}]),
    %Matches = lists:append(_Matches, [<<>>, <<>>, <<>>, <<>>, <<>>, <<>>]),
    %Origin = <<(lists:nth(1, Matches))/binary, (lists:nth(3, Matches))/binary>>,
    %Scheme = lists:nth(2, Matches),
    %Host = lists:nth(4, Matches),
    
    %_Path = lists:nth(5, Matches),
    %Path = case _Path of <<>> -> <<"/">>; P -> P end,

    %Query = lists:nth(6, Matches),
    %Hashbang = lists:nth(9, Matches),

    %Authentication not supported
    %nomatch = binary:match(Host, <<"@">>),

    {Scheme2, Auth2, Origin2, Host2, Path2, Query2, Host2, Port}.
