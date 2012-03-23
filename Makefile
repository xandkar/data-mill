REBAR=./rebar


all: clean compile link

clean:
	@$(REBAR) clean
	@rm -rf apps/*/ebin/
	@rm -rf bin/

compile:
	@$(REBAR) compile

link:
	@$(REBAR) escriptize
	@mkdir -p bin/
	@mv apps/dmill_stacker/dmill_stacker bin/
