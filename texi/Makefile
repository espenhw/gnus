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

sclean:
	rm -f gnus.*.bak gnus.ky gnus.cp gnus.fn gnus.cps gnus.kys *.log \
	gnus.log gnus.pg gnus.tp gnus.vr gnus.toc *.aux gnus.[cgk]idx \
	gnus.ilg gnus.ind gnus.[cgk]ind gnus.idx \
	gnus.tmptexi gnus.tmplatexi gnus.tmplatexi1 texput.log *.orig *.rej

clean:
	make sclean 
	rm -f *.latexi 

makeinfo: 
	makeinfo -o gnus gnus.texi	
	makeinfo -o message message.texi	

texi2latexi.elc:
	$(EMACS) -batch -l bytecomp -f batch-byte-recompile-directory

latex: gnus.texi
	$(EMACS) -batch -q -no-site-file gnus.texi -l ./texi2latex.elc -f latexi-translate

latexps: 
	egrep -v "label.*Index|chapter.*Index" gnus.latexi > gnus.tmplatexi1
	$(LATEX) gnus.tmplatexi1
	splitindex
	makeindex -o gnus.kind gnus.kidx
	makeindex -o gnus.cind gnus.cidx
	makeindex -o gnus.gind gnus.gidx
	egrep -v "end{document}" gnus.tmplatexi1 > gnus.tmplatexi
	cat postamble.tex >> gnus.tmplatexi
	$(LATEX) gnus.tmplatexi 
	$(LATEX) gnus.tmplatexi 
	$(DVIPS) -f gnus.dvi > gnus.ps

pss:	
	make latex
	make latexps

psout:
	make latex
	make latexboth
	make out

latexboth: 
	rm -f gnus-manual-a4.ps.gz gnus-manual-standard.ps.gz 
	make latexps
	mv gnus.ps gnus-manual-a4.ps
	gzip gnus-manual-a4.ps 
	sed 's/,a4paper//' gnus.latexi > gnus-standard.latexi 
	make latexps
	mv gnus.ps gnus-manual-standard.ps 
	gzip gnus-manual-standard.ps 

out:
	cp gnus-manual-standard.ps.gz gnus-manual-a4.ps.gz \
	/local/ftp/pub/emacs/gnus/manual
	mv gnus-manual-standard.ps.gz gnus-manual-a4.ps.gz \
	/hom/larsi/www_docs/gnus/manual
