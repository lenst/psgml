# Generated automatically from Makefile.in by configure.
# Makefile.in 
#
# Makefile for the GNU Emacs lisp library, PSGML
# Copyright (C) 1996 Karl Eichwalder, GPL
#
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

PACKAGE = psgml
VERSION = 1.0.2

SHELL = /bin/sh
                          # Right now we do not need it.

srcdir = .

prefix = /usr/local
datadir = ${prefix}/share
locallisppath = $(prefix)/share/emacs/site-lisp
psgmldir = $(locallisppath)/psgml
infodir = $(prefix)/info

EMACS = emacs

MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
INSTALL = /usr/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644

ELSRC =	psgml.el psgml-parse.el psgml-edit.el psgml-dtd.el \
	psgml-info.el psgml-charent.el psgml-api.el \
	psgml-other.el psgml-lucid.el 
ELC =	psgml.elc psgml-parse.elc psgml-edit.elc psgml-dtd.elc \
	psgml-info.elc psgml-charent.elc psgml-api.elc
NELC = 	psgml-other.elc
LELC = 	psgml-lucid.elc

ALL_ELC = $(ELC) $(NELC) $(LELC)

OTHERS = README.psgml iso88591.map psgml-maint.el \
ChangeLog
DOC_FILES = psgml-api.info psgml-api.texi psgml.info psgml.ps psgml.texi
AUTOCONF_FILES = Makefile.in aclocal.m4 configure configure.in \
install-sh mkinstalldirs INSTALL

DISTFILES = $(ELSRC) $(OTHERS) $(DOC_FILES) \
$(AUTOCONF_FILES)

EMACSB= $(EMACS) -batch -l psgml-maint.el


all: elc

help:
	@echo 'Use "make emacs" or "make xemacs"'

.PHONY: elc
elc:
	$(EMACSB) -f psgml-compile-files

TAGS tags:
	etags $(ELSRC) $(NSRC) $(LSRC)

install: elc installdirs install-info
	$(INSTALL_DATA) $(ELSRC) $(psgmldir)
	$(INSTALL_DATA) `$(EMACSB) -f psgml-install-elc` $(psgmldir)

installdirs:
	$(srcdir)/mkinstalldirs $(psgmldir)

install-info: psgml.info psgml-api.info
	$(INSTALL_DATA) psgml.info psgml-api.info $(infodir)
	if $(SHELL) -c 'install-info --version' \
		>/dev/null 2>&1; then \
		install-info --infodir=$(infodir) psgml.info; \
		install-info --infodir=$(infodir) psgml-api.info; \
	else true; fi


xemacs:
	$(MAKE) EMACS=xemacs all
install-xemacs:
	$(MAKE) EMACS=xemacs install

clean:
	$(RM) *~ $(ALL_ELC)
	$(RM) -r $(DISTDIR) $(DISTDIR).tar.gz $(DISTDIR).tar.gz.uu


info: psgml.info psgml-api.info

psgml.info: psgml.texi
	$(MAKEINFO) psgml.texi

psgml-api.info: psgml-api.texi
	$(MAKEINFO) psgml-api.texi

dvi: psgml.dvi psgml-api.dvi

psgml.dvi:
	$(TEXI2DVI) $(srcdir)/psgml.texi

psgml-api.dvi:
	$(TEXI2DVI) $(srcdir)/psgml-api.texi

ps: psgml.ps

psgml.ps: psgml.dvi
	dvips psgml.dvi -o psgml.ps


# Below this point are rules for release management.

DISTDIR = $(PACKAGE)-$(VERSION)

dist: Makefile disttree
	rm -f $(DISTDIR).tar.gz $(DISTDIR).tar.gz.uu
	tar cvf $(DISTDIR).tar $(DISTDIR)
	gzip -9 $(DISTDIR).tar

disttree:
	rm -rf $(DISTDIR) || exit 0
	mkdir $(DISTDIR)
	# cp $(ELFILES) $(DISTDIR)
	cp $(DISTFILES) $(DISTDIR)
	cp Makefile.def $(DISTDIR)/Makefile

# Makefile.in ends here
