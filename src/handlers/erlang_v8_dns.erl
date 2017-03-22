-module(erlang_v8_dns).

-export([run/2]).

-include_lib("kernel/src/inet_dns.hrl").

run([<<"resolve">>, Hostname, Type], _HandlerContext) ->
    Type0 = get_type(Type),
    Hostname0 = binary_to_list(Hostname),
    case inet_res:resolve(Hostname0, in, Type0) of
        {ok, #dns_rec{ anlist = Answers } = X} ->
            io:format(standard_error, "Full (~p): ~p~n", [Type0, X]),
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
get_type("CNAME") -> cname;
get_type("MX") -> mx;
get_type("SRV") -> srv;
get_type("PTR") -> ptr;
get_type("TXT") -> txt;
get_type(_) -> a.

format_answers(Type, Answers) when Type =:= a; Type =:= aaaa;
                                   Type =:= cname; Type =:= txt; Type =:= srv ->
    [#{ ttl => TTL, value => format_value(Data) } || {TTL, Data} <- Answers];
format_answers(ptr, Answers) ->
    [begin
         io:format(standard_error, "Value: ~p~n", [Value]),
         #{ ttl => TTL, value => <<>> }
     end || {TTL, Value} <- Answers];
format_answers(mx, Answers) ->
    [#{ ttl => TTL, exchange => Exchange, priority => Priority }
     || {TTL, {Priority, Exchange}} <- Answers].

format_value(Value) when is_tuple(Value) ->
    inet:ntoa(Value);
format_value(Value) ->
    Value.
