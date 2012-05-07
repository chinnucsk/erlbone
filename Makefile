PWD= `pwd`
REBAR=`which rebar || ./rebar`

.PHONY: deps

all: deps
	@$(REBAR) compile

app:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

webstart:
	exec erl -pa $(PWD)/apps/*/ebin $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s todo_web 
