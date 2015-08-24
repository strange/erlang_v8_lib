-module(erlang_v8_lib).

-export([run/1]).
-export([run/2]).

-define(TIMEOUT, 15000).

run(Source) ->
    poolboy:transaction(v8_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {run, Source}, ?TIMEOUT)
    end).

run(VM, Source) when is_binary(Source) ->
    {ok, Handlers} = application:get_env(erlang_v8_lib, handlers),
    run(VM, [{init, Source}], dict:from_list(Handlers)).

run(VM, [], _Handlers) ->
    ok = erlang_v8:reset_vm(VM),
    ok;

run(VM, [[<<"return">>, Value]|_], _Handlers) ->
    ok = erlang_v8:reset_vm(VM),
    {ok, Value};

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
                    [[callback, <<"success">>, Ref, Response]];
                {error, _Reason} ->
                    [[callback, <<"error">>, Ref, <<"bad error">>]]
            end
    end.
