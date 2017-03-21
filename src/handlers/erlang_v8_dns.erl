-module(erlang_v8_dns).

-export([run/2]).

-include_lib("kernel/src/inet_dns.hrl").

run([<<"resolve">>, Hostname, Type], _HandlerContext) ->
    Type0 = get_type(Type),
    Hostname0 = binary_to_list(Hostname),
    case inet_res:resolve(Hostname0, in, Type0) of
        {ok, #dns_rec{ anlist = Answers }} ->
            Answers0 = [{TTL, Data} || #dns_rr{ ttl = TTL, data = Data }
                                       <- Answers],
            {ok, format_answers(Type0, Answers0)};
        {error, _Reason} = Error ->
            io:format("Error: ~p~n", [Error]),
            Error
    end.

get_type(Type) when is_binary(Type) ->
    get_type(string:to_upper(binary_to_list(Type)));

get_type("A") -> a;
get_type("AAAA") -> aaaa;
get_type("MX") -> mx;
get_type(_) -> a.

format_answers(a, Answers) ->
    [#{ ttl => TTL, value => format_ip(Data) } || {TTL, Data} <- Answers].

format_ip(Value) when is_tuple(Value) ->
    inet:ntoa(Value);
format_ip(Value) ->
    io:format(standard_error, "value: ~p~n", [Value]),
    "".
