TEXISRC=../doc/psgml.texi
generated_html = about_psgml.html
dest = lyshome:homepage/about_psgml
dest2 = lyshome:/lysator/www/www-local/html/projects
destmisc = $(dest)/about_psgml

about_psgml.html: about_psgml.sgml Makefile
	osgmlnorm -c ~/lib/sgml/catalog -d about_psgml.sgml   > about_psgml.html \
		|| rm -f about_psgml.html

all: $(generated_html)

test:
	osgmlnorm -iTEST -d about_psgml.sgml > test.html

psgml.html psgml_toc.html: $(TEXISRC)
	texi2html -verbose $(TEXISRC)

clean:
	rm $(generated_html)

install: about_psgml.html
	scp about_psgml.html $(dest)
install2: about_psgml.html
	scp about_psgml.html $(dest2)



