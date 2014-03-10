REBAR=./rebar

all: app run

deps: 
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile
	
	
run:
	erl -pa ebin deps/lager/ebin/ deps/goldrush/ebin \
		deps/nksip/ebin deps/client_database/ebin

doc:

test:
	@$(REBAR) eunit skip_deps=true


