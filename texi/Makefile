TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file
INFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
LATEX=latex
DVIPS=dvips
PERL=perl

all: gnus message

most: texi2latexi.elc latex latexps

gnus: gnus.texi
	$(MAKEINFO) gnus.texi $(INFOSWI)

message: message.texi
	$(MAKEINFO) message.texi $(INFOSWI)

dvi: gnus.texi
	$(PERL) -n -e 'if (/\@iflatex/) { $$latex=1; } if (!$$latex) { print; } if (/\@end iflatex/) { $$latex=0; }' gnus.texi > gnus.tmptexi
	$(TEXI2DVI) gnus.tmptexi

refcard.dvi: refcard.tex gnuslogo.refcard gnusref.tex
	$(LATEX) refcard.tex

clean:
	rm -f gnus.*.bak gnus.ky gnus.cp gnus.fn gnus.cps gnus.kys *.log \
	gnus.log gnus.pg gnus.tp gnus.vr gnus.toc gnus.latexi *.aux gnus.cidx \
	gnus.cind gnus.ilg gnus.ind gnus.kidx gnus.kind gnus.idx \
	gnus.tmptexi gnus.tmplatexi *.latexi texput.log *.orig *.rej

makeinfo: 
	makeinfo -o gnus gnus.texi	
	makeinfo -o message message.texi	

texi2latexi.elc:
	$(EMACS) -batch -l bytecomp -f batch-byte-recompile-directory

latex: gnus.texi
	$(EMACS) -batch -q -no-site-file gnus.texi -l ./texi2latex.elc -f latexi-translate

latexps: 
	$(LATEX) gnus.latexi 
	splitindex
	makeindex -o gnus.kind gnus.kidx
	makeindex -o gnus.cind gnus.cidx
	egrep -v "end{document}|label.*Index|chapter.*Index" gnus.latexi > gnus.tmplatexi
	cat postamble.latexi >> gnus.tmplatexi
	$(LATEX) gnus.tmplatexi 
	$(DVIPS) -f gnus.dvi > gnus.ps

latexboth: 
	rm -f gnus-manual-a4.ps.gz gnus-manual-standard.ps.gz 
	make latexps
	mv gnus.ps gnus-manual-a4.ps
	gzip gnus-manual-a4.ps 
	sed 's/,a4paper//' gnus.latexi > gnus-standard.latexi 
	make latexps
	mv gnus.ps gnus-manual-standard.ps 
	gzip gnus-manual-standard.ps 

