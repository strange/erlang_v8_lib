-module(erlang_v8_http3).

-export([run/2]).

run([URL, Method, Payload], _HandlerContext) ->
    application:ensure_all_started(hackney),
    Opts = [
        {connect_timeout, 4000},
        {recv_timeout, 4000}
    ],
    Headers = [],
    Now = erlang:timestamp(),
    case hackney:request(clean_method(Method), URL, Headers, Payload, Opts) of
        {ok, Code, _RespHeaders, ClientRef} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    {ok, #{ code => Code, body => Body, time => Time }};
                {error, _Error} ->
                    {error, <<"Error reading body.">>}
            end;
        {error, nxdomain} ->
            {error, <<"Invalid domain.">>};
        {error, closed} ->
            {error, <<"HTTP Socket closed.">>};
        {error, timeout} ->
            {error, <<"HTTP request timed out">>};
        {error, connect_timeout} ->
            {error, <<"HTTP connection timed out">>};
        {error, ehostunreach} ->
            {error, <<"HTTP host not reachable">>};
        Other ->
            lager:info("Unspecified HTTP error: ~p", [Other]),
            {error, <<"Unspecified HTTP error.">>}
    end.

clean_method(<<"POST">>) -> post;
clean_method(<<"post">>) -> post;
clean_method(<<"GET">>) -> get;
clean_method(<<"get">>) -> get;
clean_method(_Other) -> get.
