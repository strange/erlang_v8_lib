-module(erlang_v8_lib_test).

-export([n/0]).
-export([n/1]).

-export([perf/0]).
-export([perf/1]).
-export([run_perf/0]).

n() -> n(100).
n(N) ->
    application:ensure_all_started(erlang_v8_lib),
    R = [eralng_v8_lib:run(<<"var x = 1; x">>) || _ <- lists:seq(1, N)],
    io:format("Ran ~p tests: ~p~n", [N, R]),
    timer:sleep(500),
    n(N).

perf() -> perf(100).

perf(N) ->
    application:ensure_all_started(erlang_v8_lib),
    test_avg(?MODULE, run_perf, [], N).

run_perf() ->
    [erlang_v8_lib:run(<<"var x = 1; x">>) || _ <- lists:seq(1, 100)].

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.
 
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
