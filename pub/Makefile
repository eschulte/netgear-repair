.PHONY: netgear-repair-preprint.pdf
.PRECIOUS: ts-cov-and-runtime-w-min.pdf fitness-improvement.pdf

all: netgear-repair-preprint.pdf

netgear-repair-preprint.pdf: netgear-repair.tex
	-latexmk -f -pdf -pdflatex="pdflatex -interactive=nonstopmode -halt-on-error -file-line-error" -use-make $<
	cp netgear-repair.pdf $@

clean:
	latexmk -C
