-module(erlang_v8_lib_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {vm, handlers}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([Files, Handlers]) ->
    process_flag(trap_exit, true),
    {ok, VM} = erlang_v8:start_vm([{file, File} || File <- Files]),
    {ok, #state{vm = VM, handlers = Handlers}}.

handle_call({run, Source, HandlerContext}, _From,
            #state{vm = VM, handlers = Handlers} = State) ->
    {ok, VMCtx} = erlang_v8_vm:create_context(VM),
    Reply = erlang_v8_lib_run:run({VMCtx, VM}, Source, Handlers,
                                  HandlerContext),
    ok = erlang_v8_vm:destroy_context(VM, VMCtx),
    {reply, Reply, State};
handle_call({run, Source, Context, HandlerContext}, _From,
            #state{vm = VM, handlers = Handlers} = State) ->
    {ok, VMCtx} = erlang_v8_vm:create_context(VM),
    {ok, _} = erlang_v8:call(VM, VMCtx, <<"__internal.setContext">>, [Context]),
    Reply = erlang_v8_lib_run:run({VMCtx, VM}, Source, Handlers, HandlerContext),
    ok = erlang_v8_vm:destroy_context(VM, VMCtx),
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
