.PHONY: all clean

ELCFILES = $(addsuffix .elc, $(basename $(wildcard *.el)))

all: $(ELCFILES)

%.elc : %.el
	@echo Compiling $<
	@emacs -batch -q -no-site-file -f batch-byte-compile $<

clean:
	@rm -f *.elc

# Use LC_ALL=C to avoid locale dependencies in the dates!
test: clean
	LC_ALL=C emacs -Q -batch -l tests/setup-unit-tests.el \
	       -l tests/ggtags-unit-tests.el \
	       -f ert-run-tests-batch-and-exit
