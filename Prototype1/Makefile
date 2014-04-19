REBAR=./rebar
DIALYZER=dialyzer
PROJECT=generic_exchange

all: app run

deps: 
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile
	
run: app
	erl -pa ebin deps/lager/ebin/ deps/goldrush/ebin \
		deps/nksip/ebin deps/client_database/ebin \
		deps/elcpcp/ebin

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib sasl tools inets crypto public_key ssl eunit

dialyze: app
	@$(DIALYZER) ebin/*.beam deps/nksip/ebin --plt .$(PROJECT).plt \
	-Werror_handling  #-Wunmatched_returns -Wrace_conditions -Wunderspecs

doc:

test: app
	@$(REBAR) eunit skip_deps=true


