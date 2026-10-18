# Top-level targets.  cabal does the Haskell side and docs/Makefile the HTML
# documentation.  make on its own is make all.
CABAL      ?= cabal
CABALFLAGS ?=

.PHONY: all build test docs clean help

all: build test docs

build:
	$(CABAL) build $(CABALFLAGS)

# After build, so that make -j never runs two cabals on dist-newstyle at once.
test: build
	$(CABAL) test $(CABALFLAGS)

docs:
	$(MAKE) -C docs html

# The HTML under docs/ is checked in for GitHub Pages, so clean leaves it
# alone.  make -C docs clean removes it.
clean:
	$(CABAL) clean

help:
	@echo "make all    - build, test and export the docs (the default)"
	@echo "make build  - build the library and the iota executable"
	@echo "make test   - build, then run the test suite"
	@echo "make docs   - export docs/*.org to HTML (needs emacs)"
	@echo "make clean  - remove cabal's build output, dist-newstyle/"
	@echo "CABALFLAGS goes to cabal build and cabal test, for example"
	@echo "  make build CABALFLAGS=--ghc-options=-Werror"
