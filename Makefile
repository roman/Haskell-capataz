################################################################################
## SETUP

.PHONY: help build test format help dev-setup lint build repl test sdist untar-sdist test-sdist clean run-example1 run-example2
.DEFAULT_GOAL := help

################################################################################
## VARIABLE

RESOLVER ?= lts-10.1

HASKELL_FILES:=$(find . -maxdepth 1 -type d | grep 'src\|test')

BIN_DIR:=./out/bin
DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
SDIST_INIT:=$$(stack init --force)

TOOLS_DIR=./tools/bin
BRITTANY_BIN:=$(TOOLS_DIR)/brittany
STYLISH_BIN:=$(TOOLS_DIR)/stylish-haskell
HLINT_BIN:=$(TOOLS_DIR)/hlint
PPSH_BIN:=$(TOOLS_DIR)/ppsh

EXAMPLE1_BIN=$(BIN_DIR)/example1
EXAMPLE2_BIN=$(BIN_DIR)/example2

BRITTANY=$(BRITTANY_BIN) --config-file .brittany.yml --write-mode inplace {} \;
STYLISH=$(STYLISH_BIN) -i {} \;
HLINT=$(HLINT_BIN) --refactor --refactor-options -i {} \;

STACK:=stack --resolver $(RESOLVER) --install-ghc --local-bin-path ./target/bin
TOOLS_STACK:=stack --stack-yaml .tools.stack.yaml --install-ghc --local-bin-path $(TOOLS_DIR)

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

################################################################################

$(HLINT_BIN):
	$(TOOLS_STACK) install hlint

$(STYLISH_BIN):
	$(TOOLS_STACK) install stylish-haskell

$(BRITTANY_BIN):
	$(TOOLS_STACK) install brittany

$(PPSH_BIN):
	$(STACK) install pretty-show

$(EXAMPLE1_BIN): $(HASKELL_FILES)
	$(STACK) build --copy-bins --local-bin-path $(BIN_DIR) --test --no-run-tests --haddock --no-haddock-deps --pedantic

$(EXAMPLE2_BIN) : $(EXAMPLE1_BIN)

.make/setup_done:
	$(TOOLS_STACK) install hlint stylish-haskell pretty-show brittany refactor
	chmod -R go-w .stack-work
	chmod go-w .ghci
	@mkdir -p .make
	@touch .make/setup_done

################################################################################

build: $(EXAMPLE1_BIN)  ## Build library and example binaries


test: $(EXAMPLE1_BIN) ## Execute test suites
	$(STACK) test

sdist: ## Build a release
	@mkdir -p target
	stack sdist . --pvp-bounds both
	cp $(SDIST_TAR) target

untar-sdist: sdist
	@mkdir -p tmp
	tar xzf $(SDIST_TAR)
	@rm -rf tmp/$(SDIST_FOLDER) || true
	mv $(SDIST_FOLDER) tmp

test_sdist: untar-sdist
	cd tmp/$(SDIST_FOLDER) && $(SDIST_INIT) && $(TEST) supervisor:supervisor-test

format: $(BRITTANY_BIN) $(STYLISH_BIN) ## Normalize style of source files
	find . -maxdepth 1 -name "*.hs" -exec $(BRITTANY) -exec $(STYLISH) && git diff --exit-code

lint: $(HLINT_BIN) ## Execute linter
	$(HLINT_BIN) $(HASKELL_FILES)

repl: $(PPSH_BIN) ## Start project's repl
	@chmod go-w -R .stack-work
	@chmod go-w .ghci
	stack ghci

clean: ## Clean built artifacts
	rm $(BIN_DIR)/*
	stack clean

dev-setup: .make/setup_done ## Install development dependencies

################################################################################
## Demo tasks

run-example1: $(EXAMPLE1_BIN) ## Runs example1 binary from tutorial
	$(EXAMPLE1_BIN) --procNumber 3

run-example2: $(EXAMPLE2_BIN) ## Runs example2 binary from tutorial
	$(EXAMPLE2_BIN) --procNumber 3
