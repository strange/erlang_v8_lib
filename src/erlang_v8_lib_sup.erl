-module(erlang_v8_lib_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type, Args),
    {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pool = ?CHILD(erlang_v8_lib_pool, worker, []),
	{ok, {{one_for_one, 10, 10}, [Pool]}}.
