ERLC_OPTIONS ?= -Wall -Werror

ERL_SRC = $(wildcard src/*.erl)
ERL_OBJ = $(patsubst src/%.erl,ebin/%.beam,$(ERL_SRC))

all: build

build: eon

eon: $(ERL_OBJ)
	./build.erl

ebin/%.beam: src/%.erl
	erlc $(ERLC_OPTIONS) -o $(dir $@) $<

clean:
	$(RM) eon examples/hello/hello
	find . -type f -name \*.beam -delete
	find . -type f -name \*.app -delete

FORCE:

.PHONY: all build
