-module(comsat_core_http_ws).
-compile(export_all).

xor_payload(Payload, Mask) -> xor_payload(Payload, Mask, <<>>).

xor_payload(<<>>, _Mask, Acc) -> Acc;
xor_payload(<<Chunk:32, Rest/binary>>, M= <<Mask:32>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:32>>);
xor_payload(<<Chunk:24, Rest/binary>>, M= <<Mask:24, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:24>>);
xor_payload(<<Chunk:16, Rest/binary>>, M= <<Mask:16, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:16>>);
xor_payload(<<Chunk:8, Rest/binary>>, M= <<Mask:8, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:8>>).

int_to_op(10) -> pong;
int_to_op(9) -> ping;
int_to_op(8) -> close;
int_to_op(2) -> binary;
int_to_op(1) -> text.

deserialize(Bin) -> deserialize_1(Bin, []).
deserialize_1(Bin, Acc) ->
    case decode_frame(Bin) of
        {incomplete, Bin} -> {Acc, Bin};
        {ok, Opcode, 0, Payload, Rest} ->
            deserialize_1(Rest, Acc ++ [{int_to_op(Opcode), Payload}])
    end.

decode_frame(<<_:1, 1:1, _:1, _:1, _:4, _/binary>>) -> {error, permessage_deflate_not_supported};
decode_frame(Chunk= <<_Fin:1, RSV1:1, _RSV2:1, _RSV3:1, 
        Opcode:4, Rest/binary>>) ->
    
    %unimplemented. Only firefox splits frames after 32kb
    %http://lucumr.pocoo.org/2012/9/24/websockets-101/
    true = 0 /= Opcode,

    case Rest of
        <<0:1, 127:7, PLen:64, P:PLen/binary, R/binary>> ->
            {ok, Opcode, RSV1, P, R};

        <<0:1, 126:7, PLen:16, P:PLen/binary, R/binary>> ->
            {ok, Opcode, RSV1, P, R};

        <<0:1, PLen:7, P:PLen/binary, R/binary>> when PLen < 126 ->
            {ok, Opcode, RSV1, P, R};

        <<1:1, 127:7, PLen:64, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};

        <<1:1, 126:7, PLen:16, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};

        <<1:1, PLen:7, M:4/binary, P:PLen/binary, R/binary>> when PLen < 126 ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};

        <<_/binary>> -> 
            {incomplete, Chunk}
    end;
decode_frame(Chunk) -> {incomplete, Chunk}.




serialize({text, Bin}) -> encode_frame(Bin, 0, 0, 1);
serialize({text, mask, Bin}) -> encode_frame(Bin, 0, 1, 1);

serialize({binary, Bin}) -> encode_frame(Bin, 0, 0, 2);
serialize({binary, mask, Bin}) -> encode_frame(Bin, 0, 1, 2);

serialize(pong) -> <<1:1, 0:1, 0:1, 0:1, 10:4, 0:1, 0:7>>;
serialize(ping) -> <<1:1, 0:1, 0:1, 0:1, 9:4, 0:1, 0:7>>;
serialize(close) -> <<1:1, 0:1, 0:1, 0:1, 8:4, 0:1, 0:7>>.

encode_frame(List, RSV1, Mask, Type) when is_list(List) -> 
    encode_frame(unicode:characters_to_binary(List), RSV1, Mask, Type);

encode_frame(Bin, RSV1, 0, Type) when byte_size(Bin) =< 125 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, (byte_size(Bin)):7, Bin/binary>>;
encode_frame(Bin, RSV1, 0, Type) when byte_size(Bin) =< 65536 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, 126:7, (byte_size(Bin)):16, Bin/binary>>;
encode_frame(Bin, RSV1, 0, Type) ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, 127:7, (byte_size(Bin)):64, Bin/binary>>;

encode_frame(Bin, RSV1, 1, Type) when byte_size(Bin) =< 125 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 1:1, (byte_size(Bin)):7, 0:32, Bin/binary>>;
encode_frame(Bin, RSV1, 1, Type) when byte_size(Bin) =< 65536 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 1:1, 126:7, (byte_size(Bin)):16, 0:32, Bin/binary>>;
encode_frame(Bin, RSV1, 1, Type) ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 1:1, 127:7, (byte_size(Bin)):64, 0:32, Bin/binary>>.