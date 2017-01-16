-module(erlang_v8_ws).

-export([run/2]).

-define(DEFAULT_RECV_TIMEOUT, 5000).
-define(RESOLVE_CONN_FUN, <<"ws.__resolve_conn_promise">>).

run([<<"connect">>, URL, Headers], HandlerContext) ->
    validate_args(#{
        url => URL,
        headers => Headers
    }, HandlerContext);

run([<<"send">>, Ref, Data], _HandlerContext) ->
    case erlang_v8_lib_bg_procs:get(Ref) of
        {error, not_found} ->
            {error, <<"No connection.">>};
        {ok, Pid} ->
            Pid ! {send, Data},
            {ok, <<"Data sent.">>}
    end;

run([<<"close">>, Ref], _HandlerContext) ->
    case erlang_v8_lib_bg_procs:get(Ref) of
        {error, not_found} ->
            {error, <<"No connection.">>};
        {ok, Pid} ->
            Pid ! close,
            receive
                ws_closed ->
                    erlang_v8_lib_bg_procs:remove(Ref),
                    {ok, <<"Socket closed.">>}
            end
    end;

run([<<"receive">>, Ref], HandlerContext) ->
    Timeout = maps:get(ws_recv_timeout, HandlerContext, ?DEFAULT_RECV_TIMEOUT),
    case erlang_v8_lib_bg_procs:get(Ref) of
        {error, not_found} ->
            {error, <<"No connection.">>};
        {ok, Pid} ->
            Pid ! read,
            receive
                {ws_frame, Frame} ->
                    {ok, Frame};
                ws_closed ->
                    {error, <<"Socket closed.">>}
                after Timeout ->
                    {error, <<"Receive timeout reached.">>}
            end
    end.

validate_args(Config, HandlerContext) ->
    case oath:validate(Config, map, #{ rules => [
            {url, string, #{ default_to_http => true }},
            {headers, map, #{ required => false, default => #{} }}
         ]}) of
        {ok, ValidConfig} ->
            validate_headers(ValidConfig, HandlerContext);
        {error, #{ url := Reason }} ->
            {error, Reason};
        {error, _Errors} ->
            {error, <<"Invalid arguments">>}
    end.

validate_headers(#{ headers := Headers } = Config, HandlerContext) ->
    _ValidHeaders = clean_headers(Headers),
    validate_url(Config, HandlerContext).

validate_url(#{ url := URL } = _Config, _HandlerContext) ->
    case clean_url(URL) of
        {ok, Transport, Hostname, Port, Path} ->
            case connect(Transport, Hostname, Port, Path) of
                {ok, Pid} ->
                    {ok, ConnRef} = erlang_v8_lib_bg_procs:add(Pid),
                    {resolve_in_js, ?RESOLVE_CONN_FUN, ConnRef};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} ->
            {error, <<"Invalid URI.">>}
    end.

clean_url(URL) ->
    Opts = [{scheme_defaults, [{ws, 80}, {wss, 443}]}],
    case http_uri:parse(URL, Opts) of
        {error, no_scheme} ->
            clean_url("ws://" ++ URL);
        {ok, {ws, _, Host, Port, Path, Query}} ->
            {ok, tcp, Host, Port, Path ++ Query};
        {ok, {wss, _, Host, Port, Path, Query}} ->
            {ok, ssl, Host, Port, Path ++ Query};
        {error, Reason} ->
            {error, Reason}
    end.

clean_headers(Headers) when is_map(Headers) ->
    case jsx:decode(jsx:encode(Headers)) of
        [{}] -> [];
        NewHeaders -> NewHeaders
    end;
clean_headers(_) ->
    [].

%% WS connection stuff

connect(Transport, Hostname, Port, Path) ->
    Parent = self(),
    Pid = spawn(fun() -> connect(Parent, Transport, Hostname, Port, Path) end),
    receive
        ok ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

connect(Parent, Transport, Hostname, Port, Path) ->
    case gun:open(Hostname, Port, #{ retry => 0, transport => Transport }) of
        {ok, Pid} ->
            case gun:await_up(Pid) of
                {ok, http} ->
                    gun:ws_upgrade(Pid, Path, [], #{ compress => true }),
                    receive
                        {gun_ws_upgrade, Pid, ok, _} ->
                            Parent ! ok,
                            loop(Pid, Parent);
                        Msg ->
                            lager:info("WS HS failed: ~p", [Msg]),
                            Parent ! {error, <<"WebSocket upgrade failed.">>}
                    end;
                {error, _Reason} ->
                    Parent ! {error, <<"Unable to connect.">>}
            end;
        {error, _Reason} ->
            Parent ! {error, <<"Unable to connect.">>}
    end.

loop(Pid, Parent) ->
    loop(Pid, Parent, [], 0).

loop(Pid, Parent, Buf, Waiting) ->
    receive
        {gun_ws, Pid, close} ->
            gun:ws_send(Pid, close),
            loop(Pid, Parent, Buf, Waiting);
        {gun_ws, Pid, {close, Code, _}} ->
            gun:ws_send(Pid, {close, Code, <<>>}),
            loop(Pid, Parent, Buf, Waiting);

        {gun_ws, Pid, {text, Frame}} when Waiting > 0 ->
            Parent ! {ws_frame, Frame},
            loop(Pid, Parent, Buf, Waiting - 1);
        {gun_ws, Pid, {text, Frame}} ->
            loop(Pid, Parent, Buf ++ Frame, Waiting);

        {gun_down, Pid, ws, _, _, _} ->
            close(Pid, Parent);

        {send, Frame} ->
            gun:ws_send(Pid, {text, Frame}),
            loop(Pid, Parent, Buf, Waiting);

        read when length(Buf) > 0 ->
            [Frame|Rest] = Buf,
            Parent ! {ws_frame, Frame},
            loop(Pid, Parent, Rest, Waiting);
        read ->
            loop(Pid, Parent, Buf, Waiting + 1);

        close ->
            close(Pid, Parent);

        Other ->
            lager:error("Unexpected ws message ~p", [Other]),
            close(Pid, Parent)
    end.

close(Pid, Parent) ->
    Parent ! ws_closed,
    gun:close(Pid),
    gun:flush(Pid).
