TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file gnus.texi -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
# MAKEINFO=makeinfo -o gnus gnus.texi
LATEX=latex

all: gnus.info refcard.dvi

gnus.info: gnus.texi
	$(MAKEINFO)

dvi: gnus.texi
	$(TEXI2DVI) gnus.texi

refcard.dvi: refcard.tex
	$(LATEX) refcard.tex
