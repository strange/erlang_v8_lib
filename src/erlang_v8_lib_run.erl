-module(erlang_v8_lib_run).

-export([run/4]).

run(VM, Source, Handlers, HandlerContext) when is_binary(Source) ->
    unwind(VM, [{init, Source}], dict:from_list(Handlers), HandlerContext).

unwind({_Context, _VM}, [], _Handlers, _HandlerContext) ->
    ok;

unwind({_Context, _VM}, [[<<"return">>, Value]|_], _Handlers, _HandlerContext) ->
    {ok, Value};

unwind({Context, VM}, [{init, Source}], Handlers, HandlerContext) ->
    case erlang_v8:eval(VM, Context, <<"
        (function() {
            __internal.actions = [];

            ", Source/binary, "

            return __internal.actions;
        })();
    ">>) of
        {ok, Actions} ->
            unwind({Context, VM}, Actions, Handlers, HandlerContext);
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        _Other ->
            {error, <<"Script error.">>}
    end;

unwind({Context, VM}, [Action|T], Handlers, HandlerContext) ->
    NewActions = case Action of
        [<<"external">>, HandlerIdentifier, Ref, Args] ->
            dispatch_external(HandlerIdentifier, Ref, Args, Handlers,
                              HandlerContext);
        [callback, Status, Ref, Args] ->
            {ok, Actions} = erlang_v8:call(VM, Context,
                                           <<"__internal.handleExternal">>,
                                           [Status, Ref, Args]),
            Actions;
        Other ->
            io:format("Other: ~p~n", [Other]),
            []
    end,
    unwind({Context, VM}, NewActions ++ T, Handlers, HandlerContext).

dispatch_external(HandlerIdentifier, Ref, Args, Handlers, HandlerContext) ->
    case dict:find(HandlerIdentifier, Handlers) of
        error ->
            {error, <<"Invalid external handler.">>};
        {ok, HandlerMod} ->
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
