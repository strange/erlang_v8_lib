-module(erlang_v8_lib_pool).

-behaviour(gen_server).

-export([start_link/1]).

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

-record(state, {
    max_contexts,
    handlers,
    vms
}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

claim() ->
    {ok, VM, Ref, Handlers} = gen_server:call(?MODULE, {claim, self()}, 2000),
    {ok, Context} = erlang_v8_vm:create_context(VM),
    ets:insert(?MODULE, {self(), VM, Context, Ref}),
    {ok, {VM, Context, Handlers}}.

release({VM, Context, _Handlers}) ->
    gen_server:call(?MODULE, {release, VM, Context}, 2000).

eval({VM, Context, _Handlers}, Source) ->
    erlang_v8:eval(VM, Context, Source).

call({VM, Context, _Handlers}, Fun, Args) ->
    erlang_v8:call(VM, Context, Fun, Args).

init([Opts]) ->
    ets:new(?MODULE, [
        duplicate_bag,
        named_table,
        public,
        {write_concurrency, true}
    ]),

    {ok, NVMs, Files, Handlers} = parse_opts(Opts),

    Args = [{file, File} || File <- Files],
    VMs = lists:map(fun(_) ->
        {ok, VM} = erlang_v8_lib_vm_sup:start_child(Args),
        VM
    end, lists:seq(1, NVMs)),

    {ok, #state{vms = VMs, handlers = Handlers}}.

handle_call({claim, Pid}, _From, #state{vms = VMs,
                                        handlers = Handlers} = State) ->
    Ref = erlang:monitor(process, Pid),
    VM = random_vm(VMs),
    {reply, {ok, VM, Ref, Handlers}, State};

handle_call({release, VM, Context}, _From, State) ->
    Pattern = {'_', VM, Context, '_'},
    case ets:match_object(?MODULE, Pattern) of
        [{_Pid, _VM, _Context, Ref}] ->
            ok = erlang_v8_vm:destroy_context(VM, Context),
            true = erlang:demonitor(Ref),
            true = ets:match_delete(?MODULE, Pattern),
            {reply, ok, State};
        [] ->
            {reply, {error, invalid_worker}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    Pattern = {Pid, '_', '_', Ref},
    case ets:match_object(?MODULE, Pattern) of
        [{Pid, VM, Context, Ref}] ->
            ok = erlang_v8_vm:destroy_context(VM, Context),
            true = ets:match_delete(?MODULE, Pattern),
            {noreply, State};
        [] ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

parse_opts(Opts) ->
    VMs = maps:get(vms, Opts, 50),

    ExtraModules = maps:get(extra_modules, Opts, []),
    Modules = maps:get(modules, Opts,
                       application:get_env(erlang_v8_lib, modules, [])),
    Core = application:get_env(erlang_v8_lib, core, []),

    Files = [begin
                 Path = priv_dir(Appname),
                 filename:join(Path, Filename)
             end || {Appname, Filename} <- Core ++ Modules ++ ExtraModules],

    DefaultHandlers = application:get_env(erlang_v8_lib, handlers, []),
    ExtraHandlers = maps:get(extra_handlers, Opts, []),
    HandlerList = erlang_v8_lib_utils:extend(1, DefaultHandlers, ExtraHandlers),
    Handlers = maps:from_list(HandlerList),

    {ok, VMs, Files, Handlers}.

random_vm(VMs) ->
    lists:nth(random:uniform(length(VMs)), VMs).

priv_dir(Appname) ->
    case code:priv_dir(Appname) of
        {error, bad_name} ->
            %% The app has probably not been loaded properly yet. Attempt to
            %% achieve the same effect by loading the priv dir relative to a
            %% module with the same name as the app. This is a terrible idea
            %% for several reasons, but the system will be replaced soon
            %% anyway.
            Ebin = filename:dirname(code:which(Appname)),
            filename:join(filename:dirname(Ebin), "priv");
        Name ->
            Name
    end.
