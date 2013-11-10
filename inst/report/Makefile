topics.tex: topics.Rtex
	R -e 'library(knitr); knit("topics.Rtex")'

topics.pdf: topics.tex
	xelatex $<

.DEFAULT_GOAL := topics.pdf

