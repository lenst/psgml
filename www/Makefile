TEXISRC=../doc/psgml.texi
generated_html = about_psgml.html
dest = sara:homepage/about_psgml
dest2 = /usr/www/html/projects
destmisc = $(dest)/about_psgml

about_psgml.html: about_psgml.sgml Makefile
	sgmlnorm -d about_psgml.sgml   > about_psgml.html  \
		|| rm -f about_psgml.html

all: $(generated_html)

test:
	sgmlnorm -iTEST -d about_psgml.sgml > test.html

psgml.html psgml_toc.html: $(TEXISRC)
	texi2html -verbose $(TEXISRC)

clean:
	rm $(generated_html)

install: about_psgml.html
	scp about_psgml.html $(dest)



