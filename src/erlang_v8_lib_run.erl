-module(erlang_v8_lib_run).

-export([run/2]).

run(Source, Opts) when is_binary(Source) ->
    HandlerContext = maps:get(handler_context, Opts, #{}),
    Handlers = maps:from_list(maps:get(handlers, Opts, [])),
    Context = maps:get(context, Opts, #{}),
    {ok, VM} = erlang_v8_lib_pool:claim(),
    {ok, _} = erlang_v8_lib_pool:call(VM, <<"__internal.setContext">>, [Context]),
    unwind(VM, [{init, Source}], Handlers, HandlerContext).

unwind(VM, [], _Handlers, _HandlerContext) ->
    ok = erlang_v8_lib_pool:release(VM),
    ok;

unwind(VM, [[<<"return">>, Value]|_], _Handlers, _HandlerContext) ->
    ok = erlang_v8_lib_pool:release(VM),
    {ok, jsx:decode(jsx:encode(Value), [return_maps])};

unwind(VM, [{init, Source}], Handlers, HandlerContext) ->
    case erlang_v8_lib_pool:eval(VM, <<"
        (function() {
            __internal.actions = [];
            ", Source/binary, "
            return __internal.actions;
        })();
    ">>) of
        {ok, Actions} ->
            unwind(VM, Actions, Handlers, HandlerContext);
        {error, Reason} ->
            {error, Reason}
    end;

unwind(VM, [Action|T], Handlers, HandlerContext) ->
    NewActions = case Action of
        [<<"external">>, HandlerIdentifier, Ref, Args] ->
            dispatch_external(HandlerIdentifier, Ref, Args, Handlers,
                              HandlerContext);
        [callback, Status, Ref, Args] ->
            {ok, Actions} = erlang_v8_lib_pool:call(VM, 
                                           <<"__internal.handleExternal">>,
                                           [Status, Ref, Args]),
            Actions;
        Other ->
            io:format("Other: ~p~n", [Other]),
            []
    end,
    unwind(VM, NewActions ++ T, Handlers, HandlerContext).

dispatch_external(HandlerIdentifier, Ref, Args, Handlers, HandlerContext) ->
    case maps:get(HandlerIdentifier, Handlers, undefined) of
        undefined ->
            [[callback, <<"error">>, Ref, <<"Invalid external handler.">>]];
        HandlerMod ->
            case HandlerMod:run(Args, HandlerContext) of
                {ok, Response} ->
                    [[callback, <<"success">>, Ref, Response]];
                ok ->
                    [[callback, <<"success">>, Ref, <<>>]];
                {error, Reason} when is_binary(Reason); is_atom(Reason) ->
                    [[callback, <<"error">>, Ref, Reason]];
                {error, _Reason} ->
                    [[callback, <<"error">>, Ref, <<"Unknown error.">>]]
            end
    end.
