srcdir = .
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
INSTALL = /usr/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644


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

