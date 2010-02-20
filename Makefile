VERSION=0.3
PKGNAME=excavator

all: emake

emake: rel
	mkdir -p ebin/
	erl -make
	
rel: release/$(PKGNAME).tar.gz
	
app:
	sh ebin/$(PKGNAME).app.in $(VERSION)
	
release/$(PKGNAME).rel release/$(PKGNAME).script release/$(PKGNAME).tar.gz: app
	mkdir -p release
	escript build_rel.escript $(PKGNAME)

test: compile
	prove t/*.t

clean:
	rm -rf $(wildcard ebin/*.beam) erl_crash.dump *.boot *.rel *.script ebin/*.app release