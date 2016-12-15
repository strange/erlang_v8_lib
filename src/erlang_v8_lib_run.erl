-module(erlang_v8_lib_run).

-export([run/2]).

run(Instructions, Opts) ->
    HandlerContext = maps:get(handler_context, Opts, #{}),
    {ok, Worker} = erlang_v8_lib_pool:claim(),
    ok = erlang_v8_lib_bg_procs:connect(),
    R = run(Worker, Instructions, HandlerContext),
    ok = erlang_v8_lib_bg_procs:disconnect(),
    _ = erlang_v8_lib_pool:release(Worker),
    R.

run(Worker, [Instruction|Instructions], HandlerContext) ->
    case unwind(Worker, [Instruction], HandlerContext) of
        {error, Reason} ->
            {error, Reason};
        Other when length(Instructions) =:= 0 ->
            Other;
        _Other ->
            run(Worker, Instructions, HandlerContext)
    end.

unwind(_Worker, [], _HandlerContext) ->
    ok;

unwind(Worker, [{context, Context}], _HandlerContext) ->
    case erlang_v8_lib_pool:call(Worker,
                                 <<"__internal.setContext">>, [Context]) of
        {error, Reason} ->
            {error, Reason};
        {ok, undefined} ->
            ok
    end;

unwind(Worker, [{call, Fun, Args}], HandlerContext) ->
    {ok, []} = erlang_v8_lib_pool:eval(Worker, <<"__internal.actions = [];">>),
    case erlang_v8_lib_pool:call(Worker, Fun, Args) of
        {error, Reason} ->
            {error, Reason};
        {ok, undefined} ->
            case erlang_v8_lib_pool:eval(Worker, <<"__internal.actions;">>) of
                {ok, Actions} ->
                    unwind(Worker, Actions, HandlerContext);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Value} ->
            %% TODO: What about returned values? Treat as a regular return?
            {ok, jsx:decode(jsx:encode(Value), [return_maps])}
    end;

unwind(Worker, [{eval, Source}], HandlerContext) ->
    case erlang_v8_lib_pool:eval(Worker, <<"
        __internal.actions = [];
        ", Source/binary, "
        __internal.actions;
    ">>) of
        {ok, Actions} ->
            unwind(Worker, Actions, HandlerContext);
        {error, Reason} ->
            {error, Reason}
    end;

unwind(_Worker, [[<<"return">>, Value]|_], _HandlerContext) ->
    {ok, jsx:decode(jsx:encode(Value), [return_maps])};

unwind(Worker, [[<<"external">>, HandlerIdentifier, Ref, Args]|T],
       HandlerContext) ->
    Actions = dispatch_external(Worker, Ref, Args, HandlerIdentifier,
                                HandlerContext),
    unwind(Worker, Actions ++ T, HandlerContext);

unwind(Worker, [[resolve_in_js, Status, Ref, Fun, Args]|T], HandlerContext) ->
    {ok, Actions} = erlang_v8_lib_pool:call(Worker, Fun, [Status, Ref, Args]),
    unwind(Worker, Actions ++ T, HandlerContext);

unwind(Worker, [[callback, Status, Ref, Args]|T], HandlerContext) ->
    Fun = <<"__internal.handleExternal">>,
    {ok, Actions} = erlang_v8_lib_pool:call(Worker, Fun, [Status, Ref, Args]),
    unwind(Worker, Actions ++ T, HandlerContext);

unwind(Worker, [Action|T], HandlerContext) ->
    lager:error("Unknown instruction: ~p", [Action]),
    unwind(Worker, T, HandlerContext).

dispatch_external({_, _, Handlers}, Ref, Args, HandlerIdentifier,
                  HandlerContext) ->
    case maps:get(HandlerIdentifier, Handlers, undefined) of
        undefined ->
            [[callback, <<"error">>, Ref, <<"Invalid external handler.">>]];
        HandlerMod ->
            case HandlerMod:run(Args, HandlerContext) of
                {resolve_in_js, Fun, Response} ->
                    [[resolve_in_js, <<"success">>, Ref, Fun, Response]];
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
