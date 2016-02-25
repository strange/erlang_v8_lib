-module(erlang_v8_lib_worker).

-behaviour(gen_server).

-export([start_link/2]).
-export([run/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {vm, handlers}).

start_link(Files, Handlers) ->
    gen_server:start_link(?MODULE, [Files, Handlers], []).

run({VM, Context}, Instructions, Opts) ->
    gen_server:call(VM, {run, {Context, Instructions, Opts}}).

%% Callbacks

init([Files, Handlers]) ->
    {ok, VM} = erlang_v8:start_vm([{file, File} || File <- Files]),
    {ok, #state{vm = VM, handlers = Handlers}}.

handle_call({run, Context, Instructions, Opts}, _From,
            #state{vm = VM, handlers = Handlers} = State) ->
    HandlerState = maps:get(handlers, Opts, []),
    Reply = run({VM, Context}, Instructions, Handlers, HandlerState),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

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
