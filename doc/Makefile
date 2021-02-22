.PHONY = all clean

all : overleaf.zip

overleaf.zip : references.bib agufull08.bst causality.tex def.tex figs
	zip -r $@ $^

clean :
	rm -rf overleaf.zip
