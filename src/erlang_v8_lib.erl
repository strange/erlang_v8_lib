-module(erlang_v8_lib).

-export([run/1]).
-export([run/2]).

run(Source) ->
    run(Source, #{}).

run(Source, Opts) when is_binary(Source) ->
    run([{eval, Source}], Opts);

run(Instructions, Opts) when is_map(Opts) ->
    erlang_v8_lib_run:run(Instructions, Opts).
