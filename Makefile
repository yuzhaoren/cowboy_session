SHELL:=/bin/sh
REBAR=./rebar

compile:
	@$(REBAR) compile

clean_local:
	@$(REBAR) clean skip_deps=true

test: clean_local compile
	@$(REBAR) ct skip_deps=true