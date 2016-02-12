-module(erlang_v8_lib).

-export([run/1]).
-export([run/2]).

run(Source) ->
    run(Source, #{}).

run(Source, Opts) when is_map(Opts) ->
    DefaultHandlers = application:get_env(erlang_v8_lib, default_handlers, []),
    LocalHandlers = application:get_env(erlang_v8_lib, local_handlers, []),
    Handlers = erlang_v8_lib_utils:extend(1, DefaultHandlers, LocalHandlers),
    NewOpts = Opts#{ handlers => Handlers },
    erlang_v8_lib_run:run(Source, NewOpts).
