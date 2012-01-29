all: deps compile

deps: deps/yaws

deps/yaws:
	@./rebar get-deps

compile:
	@./rebar compile

dialyze: all
	dialyzer -Wrace_conditions ebin

gc:
	@echo 'Removing all emacs backup files'
	@rm -f *~
	@rm -f */*~
	@rm -f */*/*~

rel: all
	@echo 'Generating dk_yaws release'
	@(cd rel; ../rebar generate)

clean: gc
	@./rebar clean

relclean:
	@rm -f rel/erl_crash.dump
	@rm -rf rel/dk_yaws

realclean: clean relclean
	@./rebar del-deps
	@rm -rf deps/*
	@rm -f erl_crash.dump
	@rm -f */erl_crash_dump


tests: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps ./rebar skip_deps=true eunit

eunit:
	make tests
