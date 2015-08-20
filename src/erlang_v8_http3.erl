-module(erlang_v8_http3).

-export([http/1]).

http([URL, MethodName, _Type, Payload]) ->
    application:ensure_all_started(hackney),
    Options = [],
    Headers = [],
    case hackney:request(clean_method(MethodName), URL, Headers,
                                    Payload, Options) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
             case hackney:body(ClientRef) of
                 {ok, Body} -> {ok, Body};
                 {error, _Error} -> {error, error_reading_body}
             end;
        Other ->
            io:format("Other: ~p~n", [Other]),
            {error, other}
    end.

clean_method(<<"post">>) -> post;
clean_method(<<"get">>) -> get;
clean_method(_Other) -> get.
