.PHONY: lisp texi elc info

all: elc texi

lisp:
	cd lisp; $(MAKE) all

texi:
	cd texi; $(MAKE) all


elc:
	cd lisp; $(MAKE) elc

info:
	cd lisp; $(MAKE) info
