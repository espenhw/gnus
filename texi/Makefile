TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file
INFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
XINFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
LATEX=latex
DVIPS=dvips
PERL=perl

all: gnus message custom widget

most: texi2latexi.elc latex latexps

gnus: gnus.texi
	$(MAKEINFO) -eval '(find-file "gnus.texi")' $(XINFOSWI)

custom: custom.texi
	$(MAKEINFO) -eval '(find-file "custom.texi")' $(XINFOSWI)

widget: widget.texi
	$(MAKEINFO) -eval '(find-file "widget.texi")' $(XINFOSWI)

message: message.texi
	$(MAKEINFO) -eval '(find-file "message.texi")' $(XINFOSWI)

dvi: gnus.dvi message.dvi

.texi.dvi :
	$(PERL) -n -e 'print unless (/\@iflatex/ .. /\@end iflatex/)' $< > gnustmp.texi
	$(TEXI2DVI) gnustmp.texi
	cp gnustmp.dvi $*.dvi
	rm gnustmp.*

refcard.dvi: refcard.tex gnuslogo.refcard gnusref.tex
	$(LATEX) refcard.tex

sclean:
	rm -f gnus.*.bak *.ky *.cp *.fn *.cps *.kys *.log *.aux *.dvi *.vr \
	*.tp *.toc \
	*.pg gnus.latexi *.aux *.[cgk]idx \
	gnus.ilg gnus.ind gnus.[cgk]ind gnus.idx \
	gnus.tmptexi gnus.tmplatexi gnus.tmplatexi1 texput.log *.orig *.rej \
	gnus.latexi*~*

clean:
	make sclean 
	rm -f *.latexi 

makeinfo: 
	makeinfo -o gnus gnus.texi	
	makeinfo -o message message.texi	
	makeinfo -o widget widget.texi	
	makeinfo -o custom custom.texi	

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

veryclean:
	make clean
	rm -f gnus.dvi gnus.ps

distclean:
	make clean
	rm -f *.orig *.rej *.elc *~ gnus gnus-[0-9] gnus-[0-9][0-9]
	rm -f message message-[0-9] widget custom
