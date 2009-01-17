all:
	rm -rf ../ebin
	mkdir ../ebin
	erlc *.erl
	mv *.beam ../ebin
	cp *.app ../ebin
	cd ../ebin/ && erl -boot start_sasl -eval 'application:load(meson_core), application:start(meson_core).'

