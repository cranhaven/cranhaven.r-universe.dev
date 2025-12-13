man_model4you.pdf: man_model4you.tex ref_model4you.bib
	pdflatex man_model4you.tex
	bibtex man_model4you
	pdflatex man_model4you.tex
	pdflatex man_model4you.tex

man_model4you.tex: man_model4you.Rnw
	Rscript -e "knitr::knit('man_model4you.Rnw')"
#	R CMD Sweave manuscript_model4you.Rnw

all: man_model4you.pdf
