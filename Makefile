EMACS=emacs

all: elc info

elc:
	cd lisp; $(emacs) -batch -l ./dgnushack.el -f dgnushack -f batch-byte-compile *.el

info:
	cd texi; makeinfo gnus.texi
