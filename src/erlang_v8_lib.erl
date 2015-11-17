-module(erlang_v8_lib).

-export([run/1]).
-export([run/2]).
-export([run/3]).
-export([run/4]).
-export([run_with_context/2]).
-export([run_with_context/3]).

-define(TIMEOUT, 15000).

run(Source) ->
    run(Source, #{}).

run(Source, HandlerContext) ->
    poolboy:transaction(v8_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {run, Source, HandlerContext}, ?TIMEOUT)
    end).

run_with_context(Source, Context) ->
    run_with_context(Source, Context, #{}).

run_with_context(Source, Context, HandlerContext) ->
    poolboy:transaction(v8_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {run, Source, Context, HandlerContext}, ?TIMEOUT)
    end).

run(VM, Source, HandlerContext) when is_binary(Source) ->
    {ok, Handlers} = application:get_env(erlang_v8_lib, handlers),
    run(VM, [{init, Source}], dict:from_list(Handlers), HandlerContext).

run(VM, [], _Handlers, _HandlerContext) ->
    ok = erlang_v8:reset_vm(VM),
    ok;

run(VM, [[<<"return">>, Value]|_], _Handlers, _HandlerContext) ->
    ok = erlang_v8:reset_vm(VM),
    {ok, Value};

run(VM, [{init, Source}], Handlers, HandlerContext) ->
    case erlang_v8:eval(VM, <<"
        (function() {
            __internal.actions = [];

            ", Source/binary, "

            return __internal.actions;
        })();
    ">>) of
        {ok, Actions} ->
            run(VM, Actions, Handlers, HandlerContext);
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        _Other ->
            {error, <<"Script error.">>}
    end;

run(VM, [Action|T], Handlers, HandlerContext) ->
    NewActions = case Action of
        [<<"external">>, HandlerIdentifier, Ref, Args] ->
            dispatch_external(HandlerIdentifier, Ref, Args, Handlers,
                              HandlerContext);
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
    run(VM, NewActions ++ T, Handlers, HandlerContext).

dispatch_external(HandlerIdentifier, Ref, Args, Handlers, HandlerContext) ->
    case dict:find(HandlerIdentifier, Handlers) of
        error ->
            {error, <<"Invalid external handler.">>};
        {ok, HandlerMod} ->
            case HandlerMod:run(Args, HandlerContext) of
                {ok, Response} ->
                    [[callback, <<"success">>, Ref, Response]];
                {error, _Reason} ->
                    [[callback, <<"error">>, Ref, <<"bad error">>]]
            end
    end.
