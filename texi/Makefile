MAKEINFO=makeinfo
TEXI2DVI=texi2dvi

all: gnus

gnus: gnus.texi
	$(MAKEINFO) -o gnus gnus.texi

dvi: gnus.texi
	$(TEXI2DVI) gnus.texi
