SOURCES=$$(find . -maxdepth 1 -type d | grep 'src\|test\|benchmark')

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
SDIST_INIT:=$$(stack init --force)

BRITTANY_BIN:=./bin/brittany
STYLISH_BIN:=./bin/stylish-haskell
HLINT_BIN:=./bin/hlint
INTERO_BIN:=./bin/intero

BRITTANY=$(BRITTANY_BIN) --config-file .brittany.yml --write-mode inplace {} \;
STYLISH=$(STYLISH_BIN) -i {} \;
HLINT=$(HLINT_BIN) --refactor --refactor-options -i {} \;

STACK:=stack --install-ghc --local-bin-path ./target/bin
TOOLS_STACK:=stack --stack-yaml .tools.stack.yaml --install-ghc --local-bin-path ./bin

TEST_DOC:=$(STACK) --haddock --no-haddock-deps build --pedantic
TEST:=$(STACK) build --test

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

################################################################################

$(HLINT_BIN):
	$(TOOLS_STACK) install hlint

$(STYLISH_BIN):
	$(TOOLS_STACK) install stylish-haskell

$(BRITTANY_BIN):
	$(TOOLS_STACK) install brittany

$(INTERO_BIN):
	$(STACK) install pretty-show intero

.make/setup_done:
	$(TOOLS_STACK) install hlint stylish-haskell pretty-show brittany
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

# pretty: bin/stylish-haskell ## Normalize style of source files
#		find $(SOURCES) -name "*.hs" -exec $(STYLISH) && git diff --exit-code
# .PHONY: pretty

format: $(BRITTANY_BIN) $(STYLISH_BIN) ## Normalize style of source files
	find $(SOURCES) -name "*.hs" -exec $(BRITTANY) -exec $(STYLISH) && git diff --exit-code
.PHONY: format

lint: $(HLINT_BIN) ## Execute linter
	hlint $(SOURCES)
.PHONY: lint

repl: $(INTERO_BIN) ## Start project's repl
	stack ghci
.PHONY: repl

setup: .make/setup_done ## Install development dependencies
.PHONY: setup
