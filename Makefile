EMACS=emacs

all: lick info

lick:
	cd lisp; $(MAKE) EMACS=$(EMACS) all

some:
	cd lisp; $(MAKE) EMACS=$(EMACS) some

info:
	cd texi; $(MAKE) EMACS=$(EMACS) all
