TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file
INFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
XINFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
LATEX=latex
DVIPS=dvips
PERL=perl
INFODIR=/usr/local/info

all: gnus message custom widget

most: texi2latex.elc latex latexps

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
	rm ps/new-herd-[0-9]*.ps

makeinfo: 
	makeinfo -o gnus gnus.texi	
	makeinfo -o message message.texi	
	makeinfo -o widget widget.texi	
	makeinfo -o custom custom.texi	

texi2latex.elc: texi2latex.el
	$(EMACS) -batch -l bytecomp -f batch-byte-recompile-directory

latex: gnus.texi
	$(EMACS) -batch -q -no-site-file gnus.texi -l ./texi2latex.elc -f latexi-translate

latexps: 
	make texi2latex.elc
	egrep -v "label.*Index|chapter.*Index" gnus.latexi > gnus.tmplatexi1
	$(LATEX) gnus.tmplatexi1
	splitindex
	makeindex -o gnus.kind gnus.kidx
	makeindex -o gnus.cind gnus.cidx
	makeindex -o gnus.gind gnus.gidx
	sed 's/\\char 5E\\relax {}/\\symbol{"5E}/' < gnus.kind > gnus.tmpkind
	mv gnus.tmpkind gnus.kind
	egrep -v "end{document}" gnus.tmplatexi1 > gnus.tmplatexi
	cat postamble.tex >> gnus.tmplatexi
	$(LATEX) gnus.tmplatexi 
	$(LATEX) gnus.tmplatexi 
	$(DVIPS) -f gnus.dvi > /local/tmp/larsi/gnus.ps

herds:
	cd ps ; for i in new-herd-[0-9]*.gif; do echo $$i; giftoppm $$i | pnmscale 2 | pnmsmooth -size 5 5 | ppmtopgm | pgmtops > `basename $$i .gif`.ps; done

pss:	
	make latex
	make latexps

psout:
	make latex
	make latexboth
	make out

screenp:
	cd screen; for i in *.gif; do echo $$i; giftopnm $$i | ppmtopgm | pnmtops -scale 1 -noturn -nocenter > `basename $$i .gif`.ps; done

latexboth: 
	rm -f gnus-manual-a4.ps.gz gnus-manual-standard.ps.gz 
	make latexps
	mv /local/tmp/larsi/gnus.ps /local/tmp/larsi/gnus-manual-a4.ps
	gzip /local/tmp/larsi/gnus-manual-a4.ps 
	sed 's/,a4paper//' gnus.latexi > gnus-standard.latexi 
	mv gnus-standard.latexi gnus.latexi
	make latexps
	mv /local/tmp/larsi/gnus.ps /local/tmp/larsi/gnus-manual-standard.ps 
	gzip /local/tmp/larsi/gnus-manual-standard.ps 

out:
	cp /local/tmp/larsi/gnus-manual-standard.ps.gz \
	/local/tmp/larsi/gnus-manual-a4.ps.gz \
	/local/ftp/pub/emacs/gnus/manual
	mv /local/tmp/larsi/gnus-manual-standard.ps.gz \
	/local/tmp/larsi/gnus-manual-a4.ps.gz \
	/hom/larsi/www_docs/www.gnus.org/documents

veryclean:
	make clean
	rm -f gnus.dvi gnus.ps

distclean:
	make clean
	rm -f *.orig *.rej *.elc *~ gnus gnus-[0-9] gnus-[0-9][0-9]
	rm -f message message-[0-9] widget custom

install:
	cp gnus gnus-[0-9] gnus-[0-9][0-9] $(INFODIR)
	cp message $(INFODIR)
	cp widget $(INFODIR)
	cp custom $(INFODIR)

