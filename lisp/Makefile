EMACS=emacs

#This is Jack's additions to reduce amount of stuff compiled
#do "make X Y.." where X Y... are the gnus packages being used.
#i.e.  "make nntp" will make gnus for nntp access only.
# virtual - adds stuff for virtual groups
# nnmbox - adds back end for mbox mail groups
# nnml - adds back end for nnml mail groups
# nnmh - adds back end for nnmh mail groups
# nnspool - adds back ends for local news spool
# mh - adds functions for using MH interface for mail and mail output

syntax:
	@echo "" ;\
	echo "*** make one or more of: nntp virtual nnmbox nnml nnmh nnspool mh all gnus" ;\
	echo "" ;\
	exit 1

nnheader.elc: nnheader.el
	$(EMACS) -batch -f batch-byte-compile $(@:.elc=.el)
gnus.elc: gnus.el
	$(EMACS) -batch -l ./nnheader.elc -f batch-byte-compile $(@:.elc=.el)
gnus-visual.elc: gnus-visual.el
	$(EMACS) -batch -f batch-byte-compile $(@:.elc=.el)
gnus-uu.elc: gnus-uu.el
	$(EMACS) -batch -l ./nnheader.elc -l ./gnus-visual.elc -l ./gnus.elc -f batch-byte-compile $(@:.elc=.el)
nntp.elc: nntp.el
	$(EMACS) -batch -l ./nnheader.elc -f batch-byte-compile $(@:.elc=.el)
nnvirtual.elc: nnvirtual.el
	$(EMACS) -batch -l ./nnheader.elc -l ./gnus-visual.elc -l ./gnus.elc -l ./nntp.elc -f batch-byte-compile $(@:.elc=.el)
nnmbox.elc: nnmbox.el
	$(EMACS) -batch -l ./nnheader.elc -l ./nnmail.elc -f batch-byte-compile $(@:.elc=.el)
nnml.elc: nnml.el
	$(EMACS) -batch -l ./nnheader.elc -l ./nnmail.elc -l ./nnheader.elc -f batch-byte-compile $(@:.elc=.el)
nnmh.elc: nnmh.el
	$(EMACS) -batch -l ./nnheader.elc -l ./gnus-visual.elc -l ./nnmail.elc -l ./gnus.elc -f batch-byte-compile $(@:.elc=.el)
nnspool.elc: nnspool.el
	$(EMACS) -batch -l ./nnheader.elc -l ./nntp.elc -f batch-byte-compile $(@:.elc=.el)
gnus-mh.elc: gnus-mh.el
	$(EMACS) -batch -l ./nnheader.elc -l ./gnus.elc -f batch-byte-compile $(@:.elc=.el)
nnmail.elc: nnmail.el
	$(EMACS) -batch -l ./nnheader.elc -f batch-byte-compile $(@:.elc=.el)
mhspool.elc: mhspool.el
	$(EMACS) -batch -l ./nnheader.elc -l ./nntp.elc -f batch-byte-compile $(@:.elc=.el)


gnus:   nnheader.elc gnus.elc gnus-visual.elc gnus-uu.elc
nntp:	gnus nntp.elc 
virtual: nntp nnvirtual.elc
nnmbox: gnus nnmail.elc  nnmbox.elc
nnml:	gnus nnmail.elc nnml.elc
nnmh:	gnus nnmail.elc nnmh.elc mhspool.elc 
nnspool: nntp nnspool.elc
mh:	gnus gnus-mh.elc
all:	nntp virtual nnmbox nnml nnmh nnspool mh

# This is what Lars wrote
# all: elc info

elc:
	$(EMACS) -batch -l ./dgnushack.el -f batch-byte-compile *.el

info:
	cd ../texi; makeinfo gnus.texi





