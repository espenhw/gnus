TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file gnus.texi -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
# MAKEINFO=makeinfo -o gnus.info gnus.texi
LATEX=latex
DVIPS=dvips

all: gnus.info refcard.dvi

most: texi2latexi.elc latex latexps

gnus.info: gnus.texi
	$(MAKEINFO)

dvi: gnus.texi
	perl -n -e ' if (/@iflatex/) { $$latex=1; } if (!$$latex) { print; } if (/@end iflatex/) { $$latex=0; }' gnus.texi > gnus.tmptexi
	$(TEXI2DVI) gnus.tmptexi

refcard.dvi: refcard.tex
	$(LATEX) refcard.tex

clean:
	rm -f gnus.*.bak gnus.ky gnus.cp gnus.aux gnus.fn gnus.dvi gnus.cps gnus.kys gnus.log gnus.pg gnus.tp gnus.vr gnus.toc

makeinfo: 
	makeinfo -o gnus.info gnus.texi	

texi2latexi.elc:
	$(EMACS) -batch -l bytecomp -f batch-byte-recompile-directory

latex: gnus.texi
	$(EMACS) -batch -q -no-site-file gnus.texi -l ./texi2latex.elc -f latexi-translate

latexps: gnus.latexi
	$(LATEX) gnus.latexi 
	splitindex
	makeindex -o gnus.kind gnus.kidx
	makeindex -o gnus.cind gnus.cidx
	egrep -v "end{document}|label.*Index|chapter.*Index" gnus.latexi > gnus.tmplatexi
	cat postamble.latexi >> gnus.tmplatexi
	$(LATEX) gnus.tmplatexi 
	$(DVIPS) -f gnus.dvi > gnus.ps

latexboth: gnus.latexi
	rm -f gnus-manual-a4.ps.gz gnus-manual-standard.ps.gz 
	$(LATEX) gnus.latexi ; $(DVIPS) -f gnus.dvi > gnus-manual-a4.ps 
	gzip gnus-manual-a4.ps 
	 sed 's/,a4paper//' gnus.latexi > gnus-standard.latexi 
	$(LATEX) gnus.latexi 
	$(DVIPS) -f gnus.dvi > gnus-manual-standard.ps 
	gzip gnus-manual-standard.ps 
