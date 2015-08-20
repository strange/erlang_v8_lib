-module(erlang_v8_http2).

-export([http/1]).

http([_URL, _MethodName, _Type, _Body]) ->
    application:ensure_all_started(shotgun),
    {ok, Conn} = shotgun:open("www.google.se", 443, https),
    Response = case shotgun:get(Conn, "/?gfe_rd=cr&amp;amp;ei=rPXSVariKoOr8weZ2oewDQ&amp;gws_rd=ssl") of
        {ok, #{body := Body}} ->
            {ok, Body};
        Other ->
            io:format("Other: ~p~n", [Other]),
            {error, other}
    end,
    shotgun:close(Conn),
    Response.
