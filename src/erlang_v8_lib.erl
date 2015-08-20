-module(erlang_v8_lib).

-export([test/0]).

test() ->
    application:ensure_all_started(erlang_v8),
    application:start(erlang_v8_lib),
    {ok, VM} = erlang_v8:start_vm([{file, "priv/base.js"}]),
    run(VM).

run(VM) ->
    run(VM, [init]).

run(_VM, []) -> ok;

run(VM, [init]) ->
    Source = <<"
        http.get('http://www.google.se').then(function(d) {
            console.log(d);
            return http.get('http://www.trell.se/');
        }).then(function(d) {
            console.log(d);
        });
    ">>,
    {ok, Actions} = erlang_v8:eval(VM, <<"
        (function() {
            __internal.actions = [];

            ", Source/binary, "

            return __internal.actions;
        })();
    ">>),
    run(VM, Actions);

run(VM, [Action|T]) ->
    NewActions = case Action of
        [<<"http">>, Ref, Args] ->
            case erlang_v8_http3:http(Args) of
                {ok, Body} ->
                    [callback, [<<"success">>, Ref, [limit_size(Body)]]];
                {error, _Reason} ->
                    [[<<"error">>, Ref, [<<"bad error">>]]]
            end;
        [callback, Status, Ref, Args] ->
            {ok, Actions} = erlang_v8:call(VM, <<"handleExternal">>, [Status, Ref, Args]),
            Actions;
        [<<"log">>, Data] ->
            io:format("Log: ~p~n", [Data]),
            [];
        Other ->
            io:format("Other: ~p~n", [Other]),
            []
    end,
    run(VM, NewActions ++ T).

-define(SIZE, 20000).

limit_size(<<S0:?SIZE/binary, _/binary>> = S) when size(S) > ?SIZE ->
    S0;
limit_size(S) ->
    S.
