REBAR=./rebar


all: clean deps compile link

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	@rm -rf apps/*/ebin/
	@rm -rf deps/*/ebin/
	@rm -rf bin/

compile:
	@$(REBAR) compile

link:
	@$(REBAR) escriptize skip_deps=true
	@mkdir -p bin/
	@mv apps/dmill_stacker/dmill_stacker bin/
