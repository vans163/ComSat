# ComSat
Erlang client for http, https, websockets, http2, quic. Proxy support for socks4/4a and socks5.

<img src="http://i.imgur.com/E84RAjH.jpg" width="960" height="600" />

### Description
ComSat is a no-magic, no behind the scenes client for common web protocols.  It is also a socks5 proxy client.  
ComSat will NEVER automatically cache responses, pool your requests or participate in other heresy.  
### Status
Placeholder  

### Roadmap
- Https  
- Websockets  
- Http2  
- QUIC  
- HTTP CONNECT Proxy
- Socks4/4a  
- Socks5  

### HTTP/HTTPS Usage
```erlang
%timeout - default 30000
%follow_redirect - default true


%comsat_http:get/1
{ok, StatusCode, Headers, Body}
    = comsat_http:get("https://www.google.com:9994/find_it?key=aaaa")

%comsat_http:get/2
comsat_http:get(Url, RequestHeaders).

%comsat_http:get/3
comsat_http:get("https://www.google.com:9994/find_it?key=aaaa", 
    #{"Cookie"=> "secret=token"}, 
    #{timeout=> 60000, follow_redirect=> false})
  

%comsat_http:post/2
{ok, StatusCode, Headers, Body}
    = comsat_http:post("https://www.google.com:9994/find_it?key=aaaa", <<"the_body">>)

%comsat_http:post/3
comsat_http:post(Url, RequestHeaders, Body).

%comsat_http:post/4
comsat_http:post("https://www.google.com:9994/find_it?key=aaaa", 
    #{"Cookie"=> "secret=token"},
    <<"the_body">>,
    #{timeout=> 60000, follow_redirect=> false})


%comsat_http:request/5
comsat_http:request(
    <<"HEAD">>, 
    "http://erlang.org/", 
    #{"Cookie"=> "secret"}, 
    <<"dont_send_me_the_body">>, 
    #{timeout=> 60000}).

```

### WS/WSS Usage
NOTE: permessage-deflate is currently not supported  

```erlang
%comsat_http:ws_connect/1
Socket = comsat_http:ws_connect("wss://google.com/app:5000").

%comsat_http:ws_connect/3
Socket = comsat_http:ws_connect("wss://google.com/app:5000", 
    #{"Sec-WebSocket-Protocol"=> "binary"}, 
    #{timeout=> 60000}).


%Recv
{ok, Chunk} = gen_tcp:recv(Socket, 0, 30000),
{[Frames], ChunkRest} = comsat_core_http_ws:deserialize(Chunk),
%Frame = {text, Binary}
%Frame = {binary, Binary}
%Frame = {ping, <<>>}
%Frame = {pong, <<>>}
%Frame = {close, <<>>}

%Send
SerializedText = comsat_core_http_ws:serialize({text, <<"hello mike">>}),
SerializedBinary = comsat_core_http_ws:serialize({binary, mask, <<"hello mike">>}),
gen_tcp:send(Socket, SerializedText),

comsat_core_http_ws:serialize(ping),
comsat_core_http_ws:serialize(pong),
comsat_core_http_ws:serialize(close)

```

### Options

timeout          Integer()  
follow_redirect  true | false  
inet_options     []  
ssl_options      []  
reuse_socket     socket() | undefined  
keep_alive       true | false  
proxy            Map() | undefined  


### Proxy Usage
```erlang
Socks5 = %{type=> socks5, host=> "1.1.1.1", port=> 8080, username=> "theuser", password=> "thepass"}
Http = %{type=> http, host=> "http://1.1.1.1:8090", username=> "theuser", password=> "thepass"}

{ok, Map} = comsat_http:get("https://www.google.com:9994/find_it?key=aaaa", 
    #{}, 
    #{keep_alive=> true, proxy=> Http, ssl_options=> [{server_name_indication, "google.com"}]})

{ok, _} = comsat_http:get("https://www.google.com:9994/find_that", 
    #{}, 
    #{keep_alive=> true, reuse_socket=> maps:get(socket, Map)})
```