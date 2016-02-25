-module(erlang_v8_http).

-export([run/2]).

run([URL, Method, Headers, Payload], _HandlerContext) ->
    application:ensure_all_started(hackney),
    Opts = [
        {connect_timeout, 6000},
        {recv_timeout, 6000}
    ],
    NewHeaders = [{<<"Connection">>, <<"close">>}|clean_headers(Headers)],
    Now = erlang:timestamp(),
    case hackney:request(clean_method(Method), URL, NewHeaders, Payload,
                         Opts) of
        {ok, Code, _RespHeaders, ClientRef} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    {ok, #{ code => Code, body => Body, time => Time }};
                {error, _Error} ->
                    {error, <<"Error reading body.">>}
            end;
        {ok, Code, _RespHeaders} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            {ok, #{ code => Code, time => Time }};
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
        {error, econnrefused} ->
            {error, <<"HTTP connection refused.">>};
        %% {error, enetunreach} ->
        %%     {error, <<"HTTP net not reachable">>};
        Other ->
            io:format("Unspecified HTTP error: ~p~n", [Other]),
            {error, <<"Unspecified HTTP error.">>}
    end.

clean_method(<<"POST">>) -> post;
clean_method(<<"post">>) -> post;
clean_method(<<"PUT">>) -> put;
clean_method(<<"put">>) -> put;
clean_method(<<"GET">>) -> get;
clean_method(<<"get">>) -> get;
clean_method(<<"DELETE">>) -> delete;
clean_method(<<"delete">>) -> delete;
clean_method(<<"HEAD">>) -> head;
clean_method(<<"head">>) -> head;
clean_method(_Other) -> get.

clean_headers(<<"{}">>) -> [];
clean_headers(Headers) -> jsx:decode(Headers).
