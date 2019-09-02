emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec

test:
	${CASK_EXEC} ${emacs} -Q -L "." -l test/run.el

.PHONY: test
