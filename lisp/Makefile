include ../Makefile.conf

.SUFFIXES:	.el .elc
.el.elc:;	 $(ELC) $<

# virtual	adds stuff for virtual groups
# nnmbox	adds back end for mbox mail groups
# nnml		adds back end for nnml mail groups
# nnmh		adds back end for nnmh mail groups
# nnspool	adds back ends for local news spool
# mh		adds functions for using MH interface for mail and mail output

SUBSETS		=gnus nntp virtual nnmbox nnml nnmh nnspool mh

DGNUS.el	=gnus-cache.el gnus-cite.el gnus-ems.el \
		gnus-kill.el gnus-mh.el gnus-msg.el gnus-score.el gnus-uu.el \
		gnus-vis.el gnus-vm.el gnus.el nnbabyl.el nndigest.el nndir.el \
		nndoc.el nnfolder.el nnheader.el nnkiboze.el nnmail.el nnmbox.el \
		nnmh.el nnml.el nnspool.el nntp.el nnvirtual.el

DGNUS.elc	=$(DGNUS.el:.el=.elc)

.PHONY:		syntax all elc $(SUBSETS)

syntax:
	@echo "make one or more of: all elc $(SUBSETS)"
	@exit 1

all:		$(DGNUS.elc)

elc:		$(DGNUS.el)
	 $(ELC) $(DGNUS.el)

install::	$(DGNUS.elc) $(DESTELC)
	$(INSTALL) $(INSTDFLAGS) $(DGNUS.elc) $(DESTELC)

install::	$(DGNUS.el) $(DESTEL)
	case "$(DESTEL)" in \
	?*) $(INSTALL) $(INSTDFLAGS) $(DGNUS.el) $(DESTEL);; \
	esac

$(DESTEL):;	$(MKDIRS) $? && chmod $(MODEDIR) $?
$(DESTELC):;	$(MKDIRS) $? && chmod $(MODEDIR) $?

gnus:		nnheader.elc gnus.elc gnus-vis.elc gnus-uu.elc
mh:		gnus gnus-mh.elc
nnmbox:		gnus nnmail.elc  nnmbox.elc
nnmh:		gnus nnmail.elc nnmh.elc
nnml:		gnus nnmail.elc nnml.elc
nntp:		gnus nntp.elc 
nnspool:	nntp nnspool.elc
virtual:	nntp nnvirtual.elc

gnus-cache.elc:	gnus.el
gnus-cite.elc:	gnus-msg.el
gnus-cite.elc:	gnus.el
gnus-kill.elc:	gnus.el
gnus-mh.elc:	gnus-msg.el
gnus-mh.elc:	gnus.el
gnus-msg.elc:	gnus.el
gnus-score.elc:	gnus.el
gnus-uu.elc:	gnus-msg.el
gnus-uu.elc:	gnus.el
#gnus-vis.elc:	gnus-easymenu.el
gnus-vis.elc:	gnus.el
gnus-vm.elc:	gnus-msg.el
gnus-vm.elc:	gnus.el
gnus.elc:	gnus-ems.el
gnus.elc:	nnheader.el
gnus.elc:	nnmh.el
gnus.elc:	nnspool.el
gnus.elc:	nntp.el
nnbabyl.elc:	nnheader.el
nnbabyl.elc:	nnmail.el
nndigest.elc:	nnheader.el
nndir.elc:	nnheader.el
nndir.elc:	nnmh.el
nndir.elc:	nnml.el
nndoc.elc:	nnheader.el
nndoc.elc:	nnmail.el
nnfolder.elc:	gnus.el
nnfolder.elc:	nnheader.el
nnfolder.elc:	nnmail.el
nnkiboze.elc:	gnus-score.el
nnkiboze.elc:	gnus.el
nnkiboze.elc:	nnheader.el
nnkiboze.elc:	nntp.el
nnmail.elc:	nnheader.el
nnmbox.elc:	nnheader.el
nnmbox.elc:	nnmail.el
nnmh.elc:	gnus.el
nnmh.elc:	nnheader.el
nnmh.elc:	nnmail.el
nnml.elc:	nnheader.el
nnml.elc:	nnmail.el
nnspool.elc:	nnheader.el
nnspool.elc:	nntp.el
nntp.elc:	nnheader.el
nnvirtual.elc:	gnus.el
nnvirtual.elc:	nnheader.el
nnvirtual.elc:	nntp.el
