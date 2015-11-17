-module(erlang_v8_http).

-export([http/2]).

http([URL, MethodName, Type, Body], _HandlerContext) ->
    HTTPOptions = [{timeout, 2000}],
    Options = [{body_format, binary}],
    Method = clean_method(MethodName),
    Req = req_for_method(Method, binary_to_list(URL), binary_to_list(Type),
                         Body),
    case httpc:request(Method, Req, HTTPOptions, Options) of
        {ok, {{_Protocol, 200, _Reason}, Headers, ResponseBody}} ->
            {ok, fix_body(Headers, ResponseBody)};
        {ok, {{_Protocol, _Other, Reason}, _Headers, _ResponseBody}} ->
            {error, list_to_binary(Reason)};
        {error, {failed_connect, _Info}} ->
            {error, <<"Failed to connect.">>};
        {error, {malformed_url, _URL}} ->
            {error, <<"Malformed URL.">>};
        {error, socket_closed_remotely} ->
            {error, <<"Socket closed remotely.">>};
        {error, timeout} ->
            {error, <<"Request timed out.">>};
        {error, _Reason} ->
            {error, <<"Unknown error.">>}
    end.

fix_body(Headers, Body) ->
    erlang:display(Headers),
    try zlib:gunzip(Body) of
        Body2 -> Body2
    catch Error:Reason ->
            io:format("ZLIB ERROR!: ~p ~p ~n", [Error, Reason]),
            Body
    end.
    %% case lists:keysearch("content-encoding", 1, Headers) of
    %%     {value, {_Key, Value}} when Value =:= "gzip" -> zlib:gunzip(Body);
    %%     _ -> Body
    %% end.

clean_method(<<"post">>) -> post;
clean_method(<<"get">>) -> get;
clean_method(_Other) -> get.

req_for_method(post, URL, Type, Body) ->
        {URL, [], Type, Body};
req_for_method(_Method, URL, _Type, _Body) ->
    {URL, []}.
