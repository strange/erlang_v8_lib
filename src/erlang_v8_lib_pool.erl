-module(erlang_v8_lib_pool).

-behaviour(gen_server).

-export([start_link/0]).

-export([claim/0]).
-export([release/1]).
-export([eval/2]).
-export([call/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {max_contexts, vms, in_use}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [10000, 50], []).

claim() ->
    gen_server:call(?MODULE, {claim, self()}, 2000).

release({VM, Context}) ->
    gen_server:call(?MODULE, {release, VM, Context}, 2000).

eval({VM, Context}, Source) ->
    erlang_v8:eval(VM, Context, Source).

call({VM, Context}, Fun, Args) ->
    erlang_v8:call(VM, Context, Fun, Args).

init([MaxContexts, NVMs]) ->
    ets:new(?MODULE, [duplicate_bag, named_table]),

    {ok, Core} = application:get_env(erlang_v8_lib, core),
    DefaultModules = application:get_env(erlang_v8_lib, default_modules, []),
    LocalModules = application:get_env(erlang_v8_lib, local_modules, []),
    Modules = [{App, Mod} || {_Key, App, Mod} <-
               erlang_v8_lib_utils:extend(1, DefaultModules, LocalModules)],
    Files = [begin
                 Path = code:priv_dir(Appname),
                 filename:join(Path, Filename)
             end || {Appname, Filename} <- Core ++ Modules],

    VMs = lists:map(fun(_) ->
        {ok, VM} = erlang_v8:start_vm([{file, File} || File <- Files]),
        VM
    end, lists:seq(1, NVMs)),

    {ok, #state{max_contexts = MaxContexts, vms = VMs, in_use = dict:new()}}.

random_vm(VMs) ->
    lists:nth(random:uniform(length(VMs)), VMs).

handle_call({claim, Pid}, _From, #state{vms = VMs} = State) ->
    Ref = erlang:monitor(process, Pid),
    VM = random_vm(VMs),
    {ok, Context} = erlang_v8_vm:create_context(VM),
    ets:insert(?MODULE, {Pid, VM, Context, Ref}),
    {reply, {ok, {VM, Context}}, State};

handle_call({release, VM, Context}, {Pid, _Ref}, State) ->
    Pattern = {Pid, '_', Context, '_'},
    [{_Pid, VM, Context, Ref}] = ets:match_object(?MODULE, Pattern),
    ok = erlang_v8_vm:destroy_context(VM, Context),
    true = erlang:demonitor(Ref),
    true = ets:match_delete(?MODULE, Pattern),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    Pattern = {Pid, '_', '_', Ref},
    [{Pid, VM, Context, Ref}] = ets:match_object(?MODULE, Pattern),
    ok = erlang_v8_vm:destroy_context(VM, Context),
    true = ets:match_delete(?MODULE, Pattern),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
