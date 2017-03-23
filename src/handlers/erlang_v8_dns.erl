-module(erlang_v8_dns).

-export([run/2]).

-include_lib("kernel/src/inet_dns.hrl").

%% TODO: Add options for alt nameservers and timeout

run([<<"resolve">>, Hostname, Type], _HandlerContext) ->
    Type0 = get_type(Type),
    Hostname0 = binary_to_list(Hostname),
    Now = erlang:timestamp(),
    case inet_res:resolve(Hostname0, in, Type0) of
        {ok, #dns_rec{ anlist = Answers }} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            Answers0 = [{TTL, Data} ||
                        #dns_rr{ ttl = TTL, data = Data, class = in, type =
                                 AnType }
                        <- Answers, AnType =:= Type0],
            {ok, #{ time => Time, answers => format_answers(Type0, Answers0) }};
        {error, {Error, _Msg}} ->
            handle_error(Error);
        {error, Error} ->
            handle_error(Error)
    end.

handle_error(nxdomain) ->
    {error, <<"Invalid domain.">>};
handle_error(timeout) ->
    {error, <<"Timeout.">>};
handle_error(servfail) ->
    {error, <<"Server failed.">>};
handle_error(refused) ->
    {error, <<"Connection refused.">>};
handle_error(Other) ->
    io:format("Other error: ~p~n", [Other]),
    {error, <<"Unknown DNS error.">>}.

get_type(Type) when is_binary(Type) ->
    get_type(string:to_upper(binary_to_list(Type)));

get_type("A") -> a;
get_type("AAAA") -> aaaa;
get_type("CNAME") -> cname;
get_type("MX") -> mx;
get_type("SRV") -> srv;
get_type("PTR") -> ptr;
get_type("TXT") -> txt;
get_type("NS") -> ns;
get_type("SOA") -> soa;
get_type("NAPTR") -> naptr;
get_type(_) -> a.

format_answers(soa, Answers) ->
    [#{ ttl => TTL, primary => Primary, email => Email, revision => Revision,
        refresh => Refresh, retry => Retry, expiration => Expiration,
        min_ttl => MinTTL  }
     || {TTL, {Primary, Email, Revision, Refresh, Retry, Expiration, MinTTL}} <- Answers];
format_answers(mx, Answers) ->
    [#{ ttl => TTL, exchange => Exchange, priority => Priority }
     || {TTL, {Priority, Exchange}} <- Answers];
format_answers(_Type, Answers)  ->
    [#{ ttl => TTL, value => format_value(Data) } || {TTL, Data} <- Answers].

format_value(Value) when is_tuple(Value) ->
    list_to_binary(inet:ntoa(Value));
format_value(Value) ->
    list_to_binary(Value).
