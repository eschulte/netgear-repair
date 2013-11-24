SHELL=bash
EMACS:=emacs
PAPER=netgear-repair

# Path to the Org-mode repository, obtainable with
#     git clone git://orgmode.org/org-mode.git
ORGPATH:=~/src/org-mode
STARTUP+=(progn (add-to-list (quote load-path) "$(ORGPATH)/lisp/")
STARTUP+=(add-to-list (quote load-path) "$(ORGPATH)/contrib/lisp/"))

BATCH_EMACS=$(EMACS) --batch --debug-init --eval '$(STARTUP)' --load init.el

%.tex: %.org %.bib
	$(BATCH_EMACS) $< -f org-latex-export-to-latex

%.pdf : %.tex
	latex $< 
	bibtex $(<:%.tex=%)
	while grep -q "Rerun to get cross" %.log; do \
		latex --output-format=pdf $<; \
	done

%.html: %.org %.bib
	$(BATCH_EMACS) $< -f org-html-export-to-html

all: $(PAPER).pdf $(PAPER).html

clean:
	rm -f $(PAPER).{,aux,bbl,blg,log,out,pdf,tex,html} *~
