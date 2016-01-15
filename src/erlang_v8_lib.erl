-module(erlang_v8_lib).

-export([run/1]).
-export([run/2]).

-define(TIMEOUT, 15000).

run(Source) ->
    run(Source, #{}, ?TIMEOUT).

run(Source, Timeout) when is_integer(Timeout) ->
    run(Source, #{}, Timeout);

run(Source, Context) when is_list(Context) ->
    run(Source, maps:from_list(Context), ?TIMEOUT);

run(Source, Context) when is_map(Context) ->
    run(Source, Context, ?TIMEOUT).

run(Source, Context, Timeout) ->
    poolboy:transaction(v8_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {run, Source, Context, #{}}, Timeout + 500)
    end).
