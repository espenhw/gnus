EMACS=emacs

all: elc info

elc:
	cd lisp; make DEMACS=$(EMACS) elc

info:
	cd texi; makeinfo gnus.texi

