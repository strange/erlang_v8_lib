-module(erlang_v8_lib_run).

-export([run/2]).

run(Instructions, Opts) ->
    DefaultHandlers = application:get_env(erlang_v8_lib, default_handlers, []),
    LocalHandlers = application:get_env(erlang_v8_lib, local_handlers, []),
    Handlers = erlang_v8_lib_utils:extend(1, DefaultHandlers, LocalHandlers),
    HandlerContext = maps:get(handler_context, Opts, #{}),
    {ok, VM} = erlang_v8_lib_pool:claim(),
    R = run(VM, Instructions, maps:from_list(Handlers), HandlerContext),
    ok = erlang_v8_lib_pool:release(VM),
    R.

run(VM, [Instruction|Instructions], Handlers, HandlerContext) ->
    case unwind(VM, [Instruction], Handlers, HandlerContext) of
        {error, Reason} ->
            {error, Reason};
        Other when length(Instructions) =:= 0 ->
            Other;
        _Other ->
            run(VM, Instructions, Handlers, HandlerContext)
    end.

unwind(_VM, [], _Handlers, _HandlerContext) ->
    ok;

unwind(VM, [{context, Context}], _Handlers, _HandlerContext) ->
    case erlang_v8_lib_pool:call(VM, <<"__internal.setContext">>, [Context]) of
        {error, Reason} ->
            {error, Reason};
        {ok, undefined} ->
            ok
    end;

unwind(VM, [{call, Fun, Args}], Handlers, HandlerContext) ->
    {ok, []} = erlang_v8_lib_pool:eval(VM, <<"__internal.actions = [];">>),
    case erlang_v8_lib_pool:call(VM, Fun, Args) of
        {error, Reason} ->
            {error, Reason};
        {ok, undefined} ->
            case erlang_v8_lib_pool:eval(VM, <<"__internal.actions;">>) of
                {ok, Actions} ->
                    unwind(VM, Actions, Handlers, HandlerContext);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Value} ->
            %% TODO: What about returned values? Treat as a regular return?
            {ok, jsx:decode(jsx:encode(Value), [return_maps])}
    end;

unwind(VM, [{eval, Source}], Handlers, HandlerContext) ->
    case erlang_v8_lib_pool:eval(VM, <<"
        __internal.actions = [];
        ", Source/binary, "
        __internal.actions;
    ">>) of
        {ok, Actions} ->
            unwind(VM, Actions, Handlers, HandlerContext);
        {error, Reason} ->
            {error, Reason}
    end;

unwind(_VM, [[<<"return">>, Value]|_], _Handlers, _HandlerContext) ->
    {ok, jsx:decode(jsx:encode(Value), [return_maps])};

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
