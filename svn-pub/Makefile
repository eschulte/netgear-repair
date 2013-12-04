# Set this to the basename of the .tex file
# This will also be the name of the generated .dvi, .ps and .pdf files
PAPER = netgear-repair

# Set this to the other source files that should be counted as 
# dependencies
OTHERSOURCES := netgear-repair.bib

all: $(PAPER).pdf

.PHONY: pdf
pdf: $(PAPER).pdf

$(PAPER).pdf: $(PAPER).tex
# Remove the .aux file because pdflatex wants it different
	rm -f $(PAPER).aux 
	if pdflatex $(PAPER).tex </dev/null; then \
		true; \
	else \
		stat=$$?; touch $(PAPER).pdf; exit $$stat; \
	fi
	bibtex $(PAPER)
	while grep "Rerun to get cross" $(PAPER).log; do \
		if pdflatex $(PAPER).tex </dev/null; then \
			true; \
		else \
			stat=$$?; touch $(PAPER).pdf; exit $$stat; \
		fi; \
	done


$(PAPER).ps: $(PAPER).pdf
	pdf2ps $(PAPER).pdf

clean:
	rm -f *.aux *.log $(PAPER).ps *.dvi *.blg *.bbl *.toc *~ *.out $(PAPER).pdf 
