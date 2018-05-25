REBAR := "./rebar3"

.PHONY: build test deps rel shell

all: deps build

build:
	$(REBAR) compile

test:
	$(REBAR) ct

deps:
	$(REBAR) get-deps

shell:
	$(REBAR) shell

rel: test
	$(REBAR) release
