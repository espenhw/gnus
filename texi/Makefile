TEXI2DVI=texi2dvi

all: gnus

gnus: gnus.texi
	emacs -batch -q -no-site-file gnus.texi -l texinfmt -f texinfo-format-buffer -f save-buffer

dvi: gnus.texi
	$(TEXI2DVI) gnus.texi
