-module(erlang_v8_http2).

-export([run/1]).

run([URL, _MethodName, _Type, _Body]) ->
    case parse_uri(URL) of
        {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} ->
            {ok, Conn} = shotgun:open(Host, Port, Scheme),
            Response = case shotgun:get(Conn, Path) of
                {ok, #{body := Body}} ->
                    {ok, Body};
                Other ->
                    io:format("Other here: ~p~n", [Other]),
                    {error, <<>>}
            end,
            shotgun:close(Conn),
            Response;
        Error ->
            io:format("Error: ~p~n", [Error]),
            {error, lol}
    end.

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
