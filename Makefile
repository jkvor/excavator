VERSION=0.0.1
PKGNAME=excavator
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`

all: app
	mkdir -p ebin/
	(cd src;$(MAKE))
	(cd t;$(MAKE))

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam ebin/*.app

rel: all
	erl -pa ebin -noshell -run excavator build_rel -s init stop
	
package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf dependencies ebin excavator include Makefile public README src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/
	
install:
	@mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	@mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/*.hrl ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
	cp *.boot $(prefix)/$(ROOTDIR)/bin/
	@mkdir -p $(prefix)/etc/init.d
	cp excavator $(prefix)/etc/init.d/