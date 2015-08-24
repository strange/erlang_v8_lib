-module(erlang_v8_lib_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {vm}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([Files]) ->
    process_flag(trap_exit, true),
    {ok, VM} = erlang_v8:start_vm([{file, File} || File <- Files]),
    {ok, #state{vm = VM}}.

handle_call({run, Source}, _From, #state{vm = VM} = State) ->
    Reply = erlang_v8_lib:run(VM, Source),
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
