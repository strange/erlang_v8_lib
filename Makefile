PROJECT = erlang_v8_lib

DEPS = erlang_v8 hackney poolboy
dep_hackney = git http://github.com/strange/hackney.git
dep_erlang_v8 = git https://github.com/strange/erlang-v8 master
dep_poolboy = git https://github.com/devinus/poolboy

include erlang.mk
