EMACS=emacs

all: elc info

elc:
	cd lisp; make EMACS=$(EMACS) elc

info:
	cd texi; makeinfo -o gnus gnus.texi

