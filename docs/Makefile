# Documentation targets.  setup.org is the shared #+SETUPFILE header, not a
# page, so it is not exported on its own.  export.el holds the export settings.
# The HTML is checked in for GitHub Pages: rebuild it with the .org change.
EMACS ?= emacs
PAGES := $(filter-out setup.org,$(wildcard *.org))

.PHONY: help html clean have-emacs

help:
	@echo "make html   - export every page to HTML (needs emacs; open index.html)"
	@echo "make clean  - remove the exported HTML"

html: $(PAGES:.org=.html)
	@echo "docs exported to *.html (open index.html)"

%.html: %.org setup.org export.el | have-emacs
	$(EMACS) --batch -Q -l ./export.el "$<" -f org-html-export-to-html

have-emacs:
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "emacs not found; install it to build the docs"; exit 1; }

clean:
	rm -f $(PAGES:.org=.html)
