SOURCES=$$(find . -maxdepth 3 -type d | grep 'src\|test\|benchmark')

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
SDIST_INIT:=$$(stack init --force)

STYLISH=stylish-haskell -i {} \;
HLINT=hlint --refactor --refactor-options -i {} \;

RESOLVER ?= lts
STACK:=stack --resolver $(RESOLVER) --install-ghc --local-bin-path ./bin

TEST_DOC:=$(STACK) --haddock --no-haddock-deps build --pedantic
TEST:=$(STACK) build --test

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

################################################################################

bin/hlint:
	$(STACK) install hlint

bin/stylish-haskell:
	$(STACK) install stylish-haskell

bin/intero:
	$(STACK) install pretty-show intero

.make/setup_done:
	$(STACK) install hlint stylish-haskell pretty-show
	mkdir -p .make
	mkdir -p .stack-work/intero
	chmod go-w .
	chmod go-w .ghci
	chmod go-w .stack-work/intero
	touch .make/setup_done

################################################################################

test: ## Execute test suites
	$(TEST_DOC) supervisor
	$(TEST) supervisor:supervisor-test
	$(TEST) supervisor:supervisor-doctest
.PHONY: test

sdist: ## Build a release
	mkdir -p target
	stack sdist . --pvp-bounds both
	cp $(SDIST_TAR) target
.PHONY: sdist

untar_sdist: sdist
	mkdir -p tmp
	tar xzf $(SDIST_TAR)
	@rm -rf tmp/$(SDIST_FOLDER) || true
	mv $(SDIST_FOLDER) tmp
.PHONY: untar_sdist

test_sdist: untar_sdist
	cd tmp/$(SDIST_FOLDER) && $(SDIST_INIT) && $(TEST) supervisor:supervisor-test
	cd tmp/$(SDIST_FOLDER) && $(TEST) supervisor:supervisor-doctest
.PHONY: test_sdist

pretty: bin/stylish-haskell ## Normalize style of source files
	find $(SOURCES) -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: pretty

lint: bin/hlint ## Execute linter
	hlint $(SOURCES)
.PHONY: lint

repl: bin/intero ## Start project's repl
	stack ghci
.PHONY: repl

setup: .make/setup_done ## Install development dependencies
.PHONY: setup
