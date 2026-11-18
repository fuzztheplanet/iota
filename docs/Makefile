# Documentation targets.  setup.org is the shared #+SETUPFILE header, not a
# page, so it is not exported on its own.  export.el holds the export settings,
# the navigation bar and the footer.  The footer shows the version, which is
# why every page also depends on ../iota.cabal.
# What is built here is not checked in.  The continuous integration workflow
# builds it with make docs and publishes it.  The one HTML file in the
# repository is iota.html, which is written by hand.
EMACS      ?= emacs
CABAL      ?= cabal
CABALFLAGS ?=
PAGES := $(filter-out setup.org,$(wildcard *.org))
API_FILES := api/*.html api/*.css api/*.js api/*.json api/*.png

.PHONY: help html api ids clean have-emacs

help:
	@echo "make html   - export every page to HTML (needs emacs; open index.html)"
	@echo "make api    - build the Haddock pages and copy them to api/ (needs cabal)"
	@echo "make ids    - give every heading without a CUSTOM_ID one, from its text"
	@echo "make clean  - remove the exported pages and api/ (iota.html stays)"

html: $(PAGES:.org=.html)
	@echo "docs exported to *.html (open index.html)"

%.html: %.org setup.org export.el ../iota.cabal | have-emacs
	$(EMACS) --batch -Q -l ./export.el "$<" -f org-html-export-to-html

# Two pages take text from other files with #+INCLUDE, so they are exported
# again when one of those files changes.
examples.html: $(wildcard ../examples/*.iota ../examples/*/*.iota ../test/golden/examples/*.golden)
guide.html: ../examples/haskell/Embed.hs ../examples/haskell/Embed.expected

# cabal haddock ends with "Documentation created:" and the directory it wrote.
# It wraps that message at spaces, so a directory with a space in its name
# comes on several lines, and the awk below puts it together again.  Only what
# a browser needs is copied: not the .haddock interface file, and not the
# copies of extra-doc-files that cabal puts next to the pages.  The pages do
# not link to the source, because hyperlinked source would add several
# megabytes and the paths of the machine that built it.  The old pages go
# first, so a module that no longer exists does not leave its page behind.
# API_FILES is everything that gets copied, which is also what clean removes.
api:
	@out=$$(cd .. && $(CABAL) haddock lib:iota $(CABALFLAGS) --haddock-quickjump) \
	  || { printf '%s\n' "$$out"; exit 1; }; \
	dir=$$(printf '%s\n' "$$out" | awk 'f { d = d sep $$0; sep = " " } /^Documentation created:/ { f = 1; d = $$0; sub(/^Documentation created: */, "", d); sep = (d == "" ? "" : " ") } END { print d }'); \
	test -f "$$dir/index.html" || { printf '%s\n' "$$out"; echo "cannot find the Haddock output"; exit 1; }; \
	mkdir -p api && \
	rm -f $(API_FILES) && \
	cp "$$dir"/*.html "$$dir"/*.css "$$dir"/*.js "$$dir"/*.json "$$dir"/*.png api/ && \
	echo "API documentation copied to api/ (open api/index.html)"

ids: | have-emacs
	for f in $(PAGES); do \
		$(EMACS) --batch -Q -l ./export.el "$$f" -f iota-docs-add-custom-ids || exit 1; \
	done

have-emacs:
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "emacs not found; install it to build the docs"; exit 1; }

# iota.html is not exported from anything.  It is the redirect that keeps
# links to the old name of guide.html working, so it is not removed here.
clean:
	rm -f $(PAGES:.org=.html)
	rm -f $(API_FILES)
	if [ -d api ]; then rmdir api; fi
