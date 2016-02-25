-module(erlang_v8_lib_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
    {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    start_link(#{}).

start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).

init([Opts]) ->
    VMs = maps:get(vms, Opts, 50),
    Children = [?CHILD(erlang_v8_lib_pool, worker, [VMs])],
	{ok, {{one_for_one, 3, 10}, Children}}.
