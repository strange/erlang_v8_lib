-module(erlang_v8_lib_pool).

-behaviour(gen_server).

-export([start_link/0]).

-export([claim/0]).
-export([release/1]).
-export([eval/2]).

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

release(Context) ->
    gen_server:call(?MODULE, {release, Context}, 2000).

eval(Context, Source) ->
    gen_server:call(?MODULE, {eval, Context, Source}, 2000).

init([MaxContexts, NVMs]) ->
    process_flag(trap_exit, true),
    Files = [],
    VMs = lists:map(fun(_) ->
        {ok, VM} = erlang_v8:start_vm([{file, File} || File <- Files]),
        VM
    end, lists:seq(1, NVMs)),
    {ok, #state{max_contexts = MaxContexts, vms = VMs, in_use = dict:new()}}.

random_vm(VMs) ->
    lists:nth(random:uniform(length(VMs)), VMs).

handle_call({claim, _Pid}, _From, #state{vms = VMs, in_use = Contexts} = State) ->
    VM = random_vm(VMs),
    {ok, Context} = erlang_v8_vm:create_context(VM),
    UpdatedContexts = dict:store(Context, VM, Contexts),
    {reply, {ok, Context}, State#state{ in_use = UpdatedContexts }};

handle_call({release, Context}, _From, #state{in_use = Contexts} = State) ->
    VM = dict:fetch(Context, Contexts),
    ok = erlang_v8_vm:destroy_context(VM, Context),
    UpdatedContexts = dict:erase(Context, Contexts),
    {reply, ok, State#state{ in_use = UpdatedContexts }};

handle_call({eval, Context, Source}, _From, #state{in_use = Contexts} = State) ->
    VM = dict:fetch(Context, Contexts),
    Reply = erlang_v8:eval(VM, Context, Source),
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
