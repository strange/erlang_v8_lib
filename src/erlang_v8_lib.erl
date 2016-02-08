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
    DefaultHandlers = application:get_env(erlang_v8_lib, default_handlers, []),
    LocalHandlers = application:get_env(erlang_v8_lib, local_handlers, []),
    Handlers = erlang_v8_lib_utils:extend(1, DefaultHandlers, LocalHandlers),
    erlang_v8_lib_run:run(Source, Handlers, Context, Timeout + 500).
