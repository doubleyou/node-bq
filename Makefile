all:
	./rebar compile skip_deps=true

full:
	./rebar get-deps update-deps compile

clean:
	./rebar clean

start: all
	erl -pa ebin -pa deps/*/ebin -s reloader -s bq
