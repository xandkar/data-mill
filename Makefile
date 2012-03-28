REBAR=./rebar


all: clean deps compile link

build: compile link

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	@rm -rf apps/*/ebin/
	@rm -rf deps/*/ebin/
	@rm -rf bin/

compile:
	@$(REBAR) compile
	@cp deps/ejson/ebin/ejson.beam          apps/datamill_stacker/ebin
	@cp deps/mochiweb/ebin/mochijson2.beam  apps/datamill_stacker/ebin

link:
	@$(REBAR) escriptize skip_deps=true
	@mkdir -p bin/
	@mv apps/datamill_stacker/datamill_stacker bin/
