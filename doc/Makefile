.PHONY = all clean

all : causality.tar.gz

causality.tar.gz : references.bib agufull08.bst causality.tex causality.bbl def.tex figs Makefile
	rm -rf $@
	tar -czvf $@ $^

overleaf.zip : references.bib agufull08.bst causality.tex def.tex figs Makefile
	rm -rf $@
	zip -r $@ $^

clean :
	rm -rf overleaf.zip
