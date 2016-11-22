-module(erlang_v8_lib_bg_procs).

-export([start_link/0]).

-export([connect/0]).
-export([disconnect/0]).
-export([add/1]).
-export([get/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% External API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_server:cast(?MODULE, {connect, self()}).

disconnect() ->
    gen_server:cast(?MODULE, {disconnect, self()}).

add(Proc) ->
    gen_server:call(?MODULE, {add, self(), Proc}).

get(Ref) ->
    gen_server:call(?MODULE, {get, self(), Ref}).

%% Callbacks

init([]) ->
    lager:info("Background process monitor started."),
    ets:new(?MODULE, [ordered_set, named_table]),
    {ok, []}.

handle_call({add, Pid, Proc}, _From, State) ->
    Ref = base64:encode(crypto:strong_rand_bytes(32)),
    Procs = ets:lookup_element(?MODULE, Pid, 3),
    ets:update_element(?MODULE, Pid, {3, Procs#{ Ref => Proc }}),
    {reply, {ok, Ref}, State};

handle_call({get, Pid, Ref}, _From, State) ->
    case ets:lookup_element(?MODULE, Pid, 3) of
        #{ Ref := Proc } ->
            {reply, {ok, Proc}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({connect, Pid}, State) ->
    MRef = erlang:monitor(process, Pid),
    ets:insert(?MODULE, {Pid, MRef, #{}}),
    {noreply, State};

handle_cast({disconnect, Pid}, State) ->
    [{_Pid, MRef, Procs}] = ets:lookup(?MODULE, Pid),
    exit_all_procs(Procs, normal),
    true = ets:delete(?MODULE, Pid),
    true = erlang:demonitor(MRef),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    [{_Pid, _MRef, Procs}] = ets:lookup(?MODULE, Pid),
    true = ets:delete(?MODULE, Pid),
    exit_all_procs(Procs, Reason),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

exit_all_procs(Procs, Reason) ->
    [exit(Pid, Reason) || Pid <- maps:values(Procs)],
    ok.
