EMACS=emacs

all: elc info

elc:
	cd lisp; $(EMACS) -batch -l ./dgnushack.el -f dgnushack -f batch-byte-compile *.el

info:
	cd texi; makeinfo gnus.texi
