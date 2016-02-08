PROJECT = erlang_v8_lib

DEPS = erlang_v8 hackney poolboy lager
dep_hackney = git http://github.com/strange/hackney.git
dep_erlang_v8 = git https://github.com/strange/erlang-v8 contexts
dep_poolboy = git https://github.com/devinus/poolboy

include erlang.mk

ERLC_OPTS += +'{parse_transform,lager_transform}'
