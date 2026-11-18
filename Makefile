# iota is built with make.  The targets wrap cabal, which does the Haskell
# side, and docs/Makefile, which builds the documentation site.  make on its
# own is make all, and make help lists the targets.  The continuous
# integration workflow runs the same targets.
CABAL        ?= cabal
# Passed on to every cabal command that builds or tests.  cabal clean, cabal
# check and cabal sdist take few of the flags the others take and do not get it.
CABALFLAGS   ?=
# What make check adds to the build: warnings are errors.  With a GHC the code
# has not been cleaned up for, turn it off with make check STRICT=
STRICT       ?= --ghc-options=-Werror
# More arguments for cabal install, as in
# make install INSTALLFLAGS=--installdir=$HOME/bin
INSTALLFLAGS ?=

.PHONY: all build test install docs api conformance check dist clean help

# cabal builds in parallel by itself, and two cabals must not work on
# dist-newstyle at the same time, so make runs one target after the other
# even under make -j.
.NOTPARALLEL:

all: build test docs

build:
	$(CABAL) build $(CABALFLAGS)

test: build
	$(CABAL) test $(CABALFLAGS)

# Puts the iota executable into cabal's install directory and says where that
# is.  An iota that is already there is replaced.
install:
	$(CABAL) install exe:iota --overwrite-policy=always $(INSTALLFLAGS) $(CABALFLAGS)

# The documentation site under docs/: the pages exported from docs/*.org and
# the Haddock pages in docs/api/.  Needs Emacs.  None of it is checked in.
docs:
	$(MAKE) -C docs html
	$(MAKE) -C docs api CABAL="$(CABAL)" CABALFLAGS="$(CABALFLAGS)"

# Only the Haddock pages.
api:
	$(MAKE) -C docs api CABAL="$(CABAL)" CABALFLAGS="$(CABALFLAGS)"

# The external R7RS test suite.  It is downloaded on first use, so this
# needs curl and the network once.  See test/conformance/README.org.
conformance: build
	test/conformance/run.sh "$$($(CABAL) list-bin exe:iota $(CABALFLAGS))"

# Everything that has to hold before a change goes in.  Warnings are errors,
# the tests do not create a golden file that is missing, the Haddock pages
# have to build and the external suite has to give the recorded result.
check:
	$(CABAL) build all --enable-tests $(STRICT) $(CABALFLAGS)
	$(CABAL) test all --enable-tests --test-show-details=direct --test-options=--no-create $(STRICT) $(CABALFLAGS)
	$(CABAL) haddock lib:iota --enable-tests $(STRICT) $(CABALFLAGS)
	test/conformance/run.sh "$$($(CABAL) list-bin exe:iota --enable-tests $(STRICT) $(CABALFLAGS))"

# The source distribution, unpacked in a temporary directory, has to build
# and pass its tests on its own.  That is what catches a file missing from
# extra-source-files or extra-doc-files in iota.cabal.
dist:
	$(CABAL) check
	@tmp=$$(mktemp -d) || exit 1; \
	trap 'rm -rf "$$tmp"' EXIT; \
	$(CABAL) sdist --builddir="$$tmp/sdist" --output-directory="$$tmp" && \
	tar -xzf "$$tmp"/iota-*.tar.gz -C "$$tmp" && \
	cd "$$tmp"/iota-*/ && \
	$(CABAL) test all --enable-tests --test-options=--no-create $(CABALFLAGS) --builddir="$$tmp/build" && \
	echo "The source distribution builds and passes its tests on its own."

# Removes what the targets above built: cabal's dist-newstyle/ and the
# documentation site.
clean:
	$(CABAL) clean
	$(MAKE) -C docs clean

help:
	@echo "make              - build, test and docs (the default)"
	@echo "make build        - build the library and the iota executable"
	@echo "make test         - build, then run the test suites"
	@echo "make install      - install the iota executable (cabal says where)"
	@echo "make docs         - build the documentation site in docs/ (needs emacs)"
	@echo "make api          - build only the Haddock pages, into docs/api/"
	@echo "make conformance  - run the external R7RS test suite (needs curl)"
	@echo "make check        - strict build, tests, Haddock and the external suite"
	@echo "make dist         - check that the source distribution builds and passes"
	@echo "make clean        - remove dist-newstyle/ and the documentation site"
	@echo "Variables: CABAL, CABALFLAGS, STRICT, INSTALLFLAGS and EMACS, for example"
	@echo "  make build CABALFLAGS=-j4"
	@echo "  make check STRICT="
	@echo "  make install INSTALLFLAGS=--installdir=\$$HOME/bin"
