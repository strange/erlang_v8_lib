-module(erlang_v8_http).

-export([run/2]).

-define(RESOLVE_FUN, <<"http.__resolve_promise">>).

run([URL, Method, Headers, Payload], HandlerContext) ->
    validate_args(#{
        url => URL,
        method => Method,
        headers => Headers,
        payload => Payload
    }, HandlerContext).

validate_args(Config, HandlerContext) ->
    case oath:validate(Config, map, #{ rules => [
            {url, url, #{ default_to_http => true }},
            {method, binary, #{}},
            {headers, map, #{ required => false, default => #{} }},
            {payload, binary, #{ required => false, default => <<>> }}
         ]}) of
        {ok, ValidConfig} ->
            validate_headers(ValidConfig, HandlerContext);
        {error, #{ url := Reason }} ->
            {error, Reason};
        {error, _Errors} ->
            {error, <<"Invalid arguments">>}
    end.

validate_headers(#{ headers := Headers } = Config, HandlerContext) ->
    ValidHeaders = clean_headers(Headers),
    UpdatedHeaders = [{<<"Connection">>, <<"close">>}|ValidHeaders],
    validate_method(Config#{ headers => UpdatedHeaders }, HandlerContext).

validate_method(#{ method := Method } = Config, HandlerContext) ->
    ValidMethod = clean_method(Method),
    perform_request(Config#{ method => ValidMethod }, HandlerContext).

perform_request(#{ url := URL, headers := Headers, payload := Payload,
                   method := Method }, _HandlerContext) ->
    Opts = [
        {connect_timeout, 6000},
        {recv_timeout, 6000}
    ],
    Now = erlang:timestamp(),
    case hackney:request(Method, URL, Headers, Payload, Opts) of
        {ok, Code, RespHeaders, ClientRef} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    {resolve_in_js, ?RESOLVE_FUN,
                     #{ code => Code, body => Body, time => Time,
                        headers => jsx:encode(RespHeaders) }};
                {error, _Error} ->
                    {error, <<"Error reading body.">>}
            end;
        {ok, Code, _RespHeaders} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            {resolve_in_js, ?RESOLVE_FUN, #{ code => Code, time => Time }};
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
        {error, enetunreach} ->
            {error, <<"Network is not reachable">>};
        {error, econnrefused} ->
            {error, <<"HTTP connection refused.">>};
        Other ->
            io:format("Unspecified HTTP error: ~p~n", [Other]),
            {error, <<"Unspecified HTTP error.">>}
    end.

clean_method(<<"POST">>) -> post;
clean_method(<<"PUT">>) -> put;
clean_method(<<"GET">>) -> get;
clean_method(<<"DELETE">>) -> delete;
clean_method(<<"HEAD">>) -> head;
clean_method(_Other) -> get.

clean_headers(Headers) when is_map(Headers) ->
    case jsx:decode(jsx:encode(Headers)) of
        [{}] -> [];
        NewHeaders -> NewHeaders
    end;
clean_headers(_) ->
    [].
