-module(erlang_v8_http2).

-export([run/1]).

run([URL, MethodName, Data]) ->
    case parse_uri(URL) of
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} ->
            {ok, Conn} = shotgun:open(Host, Port, Scheme),
            Response = case shotgun:request(Conn, parse_method(MethodName),
                                            Path ++ Query, #{}, Data, #{}) of
                {ok, #{body := Body}} ->
                    {ok, [{body, Body}]};
                {error, {timeout, _}} ->
                    {error, <<"Timeout">>};
                Other ->
                    io:format("Other here: ~p~n", [Other]),
                    {error, <<"Unknown error.">>}
            end,
            shotgun:close(Conn),
            Response;
        Error ->
            io:format("Error: ~p~n", [Error]),
            {error, lol}
    end.

parse_method(<<"GET">>) ->
    get;
parse_method(<<"POST">>) ->
    post;
parse_method(_) ->
    get.

parse_uri(URI) when is_binary(URI) ->
    parse_uri(binary_to_list(URI));
parse_uri(URI) ->
    case http_uri:parse(URI) of
        {error, {not_supported_scheme , _}} ->
            parse_uri("http://" ++ URI);
        {error, no_scheme} ->
            parse_uri("http://" ++ URI);
        {error, {malformed_url, _, _}} ->
            case string:tokens(URI, ":") of
                Parts when length(Parts) =:= 2 ->
                    parse_uri("http://" ++ URI);
                _ ->
                    {error, invalid_uri}
            end;
        {ok, _} = Result ->
            Result;
        _Other ->
            {error, invalid_uri}
    end.
