all:
	mkdir -p ebin/
	#(cd templates;$(MAKE))
	(cd src;$(MAKE))

test: all
	prove -v t/*.t

clean:
	(cd templates;$(MAKE) clean)
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam

rel: all
	erl -pa ebin -noshell -run excavator build_rel -s init stop