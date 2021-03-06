.PHONY: vendor binary clean deploy update-deps

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

bin/necro: $(lisps)
	sbcl --load "src/build.lisp"

binary: bin/necro

# Clean -----------------------------------------------------------------------

clean:
	rm -rf bin

# Deploy ----------------------------------------------------------------------

# Server --------------
update-deps:
	hg -R /home/sjl/lib/cl-losh -v pull -u
	hg -R /home/sjl/lib/chancery -v pull -u
	hg -R /home/sjl/lib/cl-pcg -v pull -u
	git -C /home/sjl/lib/cl-charms pull --ff-only origin dev

/opt/necro/necro: update-deps bin/necro
	rm -f /opt/necro/necro
	cp bin/necro /opt/necro/necro

# Local ---------------
deploy: bin/necro
	rsync --exclude=bin --exclude=.hg -avz . jam:/home/sjl/src/necro
	ssh jam make -C /home/sjl/src/necro /opt/necro/necro
