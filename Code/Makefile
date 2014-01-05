REBAR=./rebar

all: app run

deps: 
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

	
run:
	erl -pa generic_switch/ebin deps/lager/ebin/ deps/goldrush/ebin \
		deps/nksip/ebin


