REBAR = rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check:
	@$(REBAR) do proper -v

shell:
	@$(REBAR) shell

doc:
	@$(REBAR) edoc

.PHONY: compile clean dialyzer check shell doc
