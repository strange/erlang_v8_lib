PROJECT = erlang_v8_lib

DEPS = erlang_v8 shotgun poolboy
dep_shotgun = https://github.com/inaka/shotgun
dep_erlang_v8 = https://github.com/strange/erlang-v8
dep_poolboy = https://github.com/devinus/poolboy

include erlang.mk
