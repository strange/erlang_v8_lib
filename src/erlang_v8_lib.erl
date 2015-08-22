-module(erlang_v8_lib).

-export([test/0]).
-export([run/2]).

-define(TIMEOUT, 15000).

test() ->
    application:start(erlang_v8_lib),
    Source = <<"
        http.get('http://www.google.se').then(function(d) {
            console.log('here');
            return http.get('http://www.trell.se/');
        }).then(function(d) {
            console.log(Math.random());
        });

        http.get('http://www.google.se').then(function(d) {
            return http.get('http://www.xfsdatrell.se/');
        }).catch(function(e) {
            console.log('error');
        });
    ">>,
    run(Source).

run(Source) ->
    poolboy:transaction(v8_worker_pool, fun(Worker) ->
        io:format("Worker: ~p~n", [Worker]),
        gen_server:call(Worker, {run, Source}, ?TIMEOUT)
    end).

run(VM, Source) when is_binary(Source) ->
    {ok, Handlers} = application:get_env(erlang_v8_lib, handlers),
    run(VM, [{init, Source}], dict:from_list(Handlers)).

run(_VM, [], _Handlers) -> ok;

run(VM, [{init, Source}], Handlers) ->
    {ok, Actions} = erlang_v8:eval(VM, <<"
        (function() {
            __internal.actions = [];

            ", Source/binary, "

            return __internal.actions;
        })();
    ">>),
    run(VM, Actions, Handlers);

run(VM, [Action|T], Handlers) ->
    NewActions = case Action of
        [<<"external">>, HandlerIdentifier, Ref, Args] ->
            dispatch_external(HandlerIdentifier, Ref, Args, Handlers);
        [callback, Status, Ref, Args] ->
            {ok, Actions} = erlang_v8:call(VM, <<"__internal.handleExternal">>,
                                           [Status, Ref, Args]),
            Actions;
        [<<"log">>, Data] ->
            io:format("Log: ~p~n", [Data]),
            [];
        Other ->
            io:format("Other: ~p~n", [Other]),
            []
    end,
    run(VM, NewActions ++ T, Handlers).

dispatch_external(HandlerIdentifier, Ref, Args, Handlers) ->
    case dict:find(HandlerIdentifier, Handlers) of
        error ->
            {error, <<"Invalid external handler.">>};
        {ok, HandlerMod} ->
            case HandlerMod:run(Args) of
                {ok, Response} ->
                    [[callback, <<"success">>, Ref, [Response]]];
                {error, _Reason} ->
                    [[callback, <<"error">>, Ref, [<<"bad error">>]]]
            end
    end.

%% -define(SIZE, 20000).
%%
%% limit_size(<<S0:?SIZE/binary, _/binary>> = S) when size(S) > ?SIZE ->
%%     S0;
%% limit_size(S) ->
%%     S.
