-module(comsat_core_uri).
-compile(export_all).



parse(Url) ->
    {match, _Matches} = re:run(Url, 
        "^(([^:\\/?#]+):)?(\\/\\/([^\/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?", 
        [{capture, all_but_first, binary}]),
    Matches = lists:append(_Matches, [<<>>, <<>>, <<>>, <<>>, <<>>, <<>>]),
    Origin = <<(lists:nth(1, Matches))/binary, (lists:nth(3, Matches))/binary>>,
    Scheme = lists:nth(2, Matches),
    Host = lists:nth(4, Matches),
    
    _Path = lists:nth(5, Matches),
    Path = case _Path of <<>> -> <<"/">>; P -> P end,

    Query = lists:nth(6, Matches),
    %Hashbang = lists:nth(9, Matches),

    %Authentication not supported
    nomatch = binary:match(Host, <<"@">>),

    case binary:split(Host, <<":">>) of
        [DNSName, PortBin] ->
            PortInt = binary_to_integer(PortBin),
            {Scheme, <<>>, Origin, Host, Path, Query, DNSName, PortInt};

        [DNSName] when Scheme == <<"http">> ->
            {Scheme, <<>>, Origin, Host, Path, Query, DNSName, 80};

        [DNSName] when Scheme == <<"https">> ->
            {Scheme, <<>>, Origin, Host, Path, Query, DNSName, 443};

        [DNSName] when Scheme == <<"ws">> ->
            {Scheme, <<>>, Origin, Host, Path, Query, DNSName, 80};

        [DNSName] when Scheme == <<"wss">> ->
            {Scheme, <<>>, Origin, Host, Path, Query, DNSName, 443}
    end.
