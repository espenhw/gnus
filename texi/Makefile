TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file gnus.texi -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
# MAKEINFO=makeinfo -o gnus.info gnus.texi
LATEX=latex

all: gnus.info refcard.dvi

gnus.info: gnus.texi
	$(MAKEINFO)

dvi: gnus.texi
	$(TEXI2DVI) gnus.texi

refcard.dvi: refcard.tex
	$(LATEX) refcard.tex

clean:
	rm -f gnus.*.bak gnus.ky gnus.cp gnus.aux gnus.fn gnus.dvi gnus.cps gnus.kys gnus.log gnus.pg gnus.tp gnus.vr gnus.toc

makeinfo: 
	makeinfo -o gnus.info gnus.texi	
