SHELL = /bin/sh
EMACS=emacs
FLAGS=-batch -q -no-site-file -l ./dgnushack.el

all:
	rm -f *.elc ; $(EMACS) $(FLAGS) -f dgnushack-compile

some:
	$(EMACS) $(FLAGS) -f dgnushack-recompile

tags:
	etags *.el

separately:
	rm -f *.elc ; for i in *.el; do $(EMACS) $(FLAGS) -f batch-byte-compile $$i; done
