SHELL = /bin/sh
EMACS=emacs
FLAGS=-batch -q -no-site-file -l ./dgnushack.el

total:
	rm -f *.elc ; $(EMACS) $(FLAGS) -f dgnushack-compile

all:
	rm -f *.elc ; $(EMACS) $(FLAGS) -f dgnushack-compile

clever:
	$(EMACS) $(FLAGS) -f dgnushack-compile

some:
	$(EMACS) $(FLAGS) -f dgnushack-recompile

tags:
	etags *.el

separately:
	rm -f *.elc ; for i in *.el; do $(EMACS) $(FLAGS) -f batch-byte-compile $$i; done

pot:
	xpot -drgnus -r`cat ./version` *.el > rgnus.pot
