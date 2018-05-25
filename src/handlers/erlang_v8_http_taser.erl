-module(erlang_v8_http_taser).

-export([run/2]).

-define(RESOLVE_FUN, <<"http.__resolve_promise">>).

run([URL, Method, Headers, Payload, FollowRedirect], HandlerContext) ->
    validate_args(#{
        url => URL,
        method => Method,
        headers => Headers,
        payload => Payload,
        follow_redirect => FollowRedirect
    }, HandlerContext).

validate_args(Config, HandlerContext) ->
    case oath:validate(Config, map, #{ rules => [
            {url, url, #{ default_to_http => true }},
            {method, binary, #{}},
            {headers, map, #{ required => false, default => #{} }},
            {payload, binary, #{ required => false, default => <<>> }},
            {follow_redirect, any, #{ required => false, default => false,
                                       in => [true, false] }}
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
    validate_method(Config#{ headers => ValidHeaders }, HandlerContext).

validate_method(#{ method := Method } = Config, HandlerContext) ->
    ValidMethod = clean_method(Method),
    perform_request(Config#{ method => ValidMethod }, HandlerContext).

perform_request(#{ url := URL, headers := Headers, payload := Payload,
                   method := Method, follow_redirect := FollowRedirect },
                _HandlerContext) ->
    Opts = #{
        connect_timeout => 6000,
        response_timeout => 10000,
        data => Payload,
        follow_redirects => FollowRedirect
    },
    Now = erlang:timestamp(),
    case catch taser:request(Method, URL, Headers, Opts) of
        {ok, Code, RespHeaders, Body} ->
            Time = timer:now_diff(erlang:timestamp(), Now) / 1000,
            Response = #{ code => Code, body => Body, time => Time,
                          headers => jsx:encode(RespHeaders) },
            {resolve_in_js, ?RESOLVE_FUN, Response};
        {error, nxdomain} ->
            {error, <<"Invalid domain.">>};
        {error, closed} ->
            {error, <<"HTTP Socket closed.">>};
        {error, timeout} ->
            {error, <<"HTTP request timed out">>};
        {error, connect_timeout} ->
            {error, <<"HTTP connection timed out">>};
        {error, response_timeout} ->
            {error, <<"HTTP server did not respond in time">>};
        {error, body_timeout} ->
            {error, <<"HTTP server did not send entire body in time">>};
        {error, ehostunreach} ->
            {error, <<"HTTP host not reachable">>};
        {error, enetunreach} ->
            {error, <<"Network is not reachable">>};
        {error, econnrefused} ->
            {error, <<"HTTP connection refused.">>};
        Other ->
            lager:error("Unspecified HTTP error (~p): ~p", [URL, Other]),
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
