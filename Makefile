################################################################################
## SETUP

.PHONY: help build test format help dev-setup lint build repl test sdist untar-sdist test-sdist clean run-example1 run-example2
.DEFAULT_GOAL := help

################################################################################
## VARIABLE

PROJECT_VERSION:=$(shell cat package.yaml | grep -v '\#' | grep version | awk '{print $$2}' | sed -e "s;'\(.*\)';\1;")
RESOLVER ?= $(shell cat stack.yaml | grep -v '\#' | grep resolver | awk '{print $$2}')

FIND_HASKELL_SOURCES=find . -name "*.hs" -not -path '*.stack-work*'
HASKELL_FILES:=$(shell $(FIND_HASKELL_SOURCES) | grep 'src\|test')

BIN_DIR:=./out/bin

STACK_DIST_DIR:=$(shell stack path --dist-dir)
SDIST_DIR_NAME:=capataz-$(PROJECT_VERSION)
INTERNAL_SDIST_TAR:=$(STACK_DIST_DIR)/$(SDIST_DIR_NAME).tar.gz

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
NIGHTLY_STACK:=stack --resolver nightly --install-ghc
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
	$(STACK) test --dump-logs

sdist: clean ## Build a release
	@mkdir -p target
	$(NIGHTLY_STACK) sdist . --pvp-bounds both
	cp $(INTERNAL_SDIST_TAR) target

untar-sdist: sdist
	@mkdir -p tmp
	tar xzf $(INTERNAL_SDIST_TAR)
	@rm -rf tmp/$(SDIST_DIR_NAME) || true
	mv $(SDIST_DIR_NAME) tmp

test-sdist: untar-sdist
	cd tmp/$(SDIST_DIR_NAME) && $(NIGHTLY_STACK) init --force && $(NIGHTLY_STACK) build --test --bench --haddock --no-run-benchmarks

format: $(BRITTANY_BIN) $(STYLISH_BIN) ## Normalize style of source files
	$(FIND_HASKELL_SOURCES) -exec $(BRITTANY) -exec $(STYLISH) && git diff --exit-code

lint: $(HLINT_BIN) ## Execute linter
	$(HLINT_BIN) $$($(FIND_HASKELL_SOURCES))

repl: $(PPSH_BIN) ## Start project's repl
	@chmod go-w -R .stack-work
	@chmod go-w .ghci
	stack ghci

clean: ## Clean built artifacts
	rm -f $(BIN_DIR)/*
	rm -f target/*
	rm -rf tmp/*
	stack clean

dev-setup: .make/setup_done ## Install development dependencies

################################################################################
## Demo tasks

run-example1: $(EXAMPLE1_BIN) ## Runs example1 binary from tutorial
	$(EXAMPLE1_BIN) --procNumber 3

run-example2: $(EXAMPLE2_BIN) ## Runs example2 binary from tutorial
	$(EXAMPLE2_BIN) --procNumber 3
