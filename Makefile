EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -batch -L . -L test -l test/wikipedia-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile wikipedia-*.el

clean:
	rm -f *.elc test/*.elc
