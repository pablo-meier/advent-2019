

run: compile
	erl -pa _build/default/lib/day9/ebin -noshell -s day9_app start -s erlang halt

compile:
	rebar3 compile

clean:
	rebar3 clean
	rm -rf _build
