EMACS=emacs
FLAGS=-batch -q -no-site-file -l ./dgnushack.el

all:
	rm *.elc ; $(EMACS) $(FLAGS) -f batch-byte-compile *.el

some:
	$(EMACS) $(FLAGS) -f dgnushack-recompile


