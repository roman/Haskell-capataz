################################################################################
## SETUP

.PHONY: help build test format help dev-setup lint build repl test sdist untar-sdist test-sdist clean run-example1 run-example2
.DEFAULT_GOAL := help

################################################################################
## VARIABLE

PROJECT_NAME:=$(shell cat package.yaml | grep 'name:' | awk '{print $$2}')
PROJECT_VERSION:=$(shell cat package.yaml | grep -v '\#' | grep version | awk '{print $$2}' | sed -e "s;'\(.*\)';\1;")
RESOLVER ?= $(shell cat stack.yaml | grep -v '\#' | grep resolver | awk '{print $$2}')
PROJECT_SETUP_FILE=./.make/setup_done

FIND_HASKELL_FILES=find . -name "*.hs" -not -path '*.stack-work*'
HASKELL_FILES:=$(shell $(FIND_HASKELL_FILES) | grep 'src\|test')

PROJECT_BIN_DIR:=./out/bin

SDIST_DIR_NAME:=$(PROJECT_NAME)-$(PROJECT_VERSION)
INTERNAL_SDIST_TAR:=$(shell stack path --dist-dir)/$(SDIST_DIR_NAME).tar.gz
PROJECT_SDIST_TAR=target/$(SDIST_DIR_NAME).tar.gz

PROJECT_TOOLS_DIR=./tools/bin
BRITTANY_BIN:=$(PROJECT_TOOLS_DIR)/brittany
STYLISH_BIN:=$(PROJECT_TOOLS_DIR)/stylish-haskell
HLINT_BIN:=$(PROJECT_TOOLS_DIR)/hlint
PPSH_BIN:=$(PROJECT_TOOLS_DIR)/ppsh
REFACTOR_BIN:=$(PROJECT_TOOLS_DIR)/refactor

EXAMPLE1_BIN=$(PROJECT_BIN_DIR)/example1
EXAMPLE2_BIN=$(PROJECT_BIN_DIR)/example2

BRITTANY_FIND_EXEC=$(BRITTANY_BIN) --config-file .brittany.yml --write-mode inplace {} \;
STYLISH_FIND_EXEC=$(STYLISH_BIN) -i {} \;
HLINT_FIND_EXEC=$(HLINT_BIN) --with-refactor=$$(pwd)/$(REFACTOR_BIN) --refactor --refactor-options -i {} \;

STACK:=stack --resolver $(RESOLVER) --install-ghc --local-bin-path ./target/bin
NIGHTLY_STACK:=stack --resolver nightly --install-ghc
TOOLS_STACK:=stack --stack-yaml .tools.stack.yaml --install-ghc --local-bin-path $(PROJECT_TOOLS_DIR)

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

################################################################################

$(EXAMPLE1_BIN): $(HASKELL_FILES)
	$(STACK) build --copy-bins --local-bin-path $(PROJECT_BIN_DIR) --test --no-run-tests --haddock --no-haddock-deps --pedantic

$(EXAMPLE2_BIN) : $(EXAMPLE1_BIN)

$(INTERNAL_SDIST_TAR):
	@mkdir -p target
	$(NIGHTLY_STACK) sdist . --pvp-bounds both

$(PROJECT_SDIST_TAR): $(INTERNAL_SDIST_TAR)
	cp $(INTERNAL_SDIST_TAR) target


$(PROJECT_SETUP_FILE):
	$(TOOLS_STACK) install hlint stylish-haskell pretty-show brittany apply-refact
	chmod -R go-w .stack-work
	chmod go-w .ghci
	@mkdir -p .make
	@touch $(PROJECT_SETUP_FILE)

################################################################################

build: $(EXAMPLE1_BIN)  ## Build library and example binaries

test: $(EXAMPLE1_BIN) ## Execute test suites
	$(STACK) test --dump-logs

sdist: $(PROJECT_SDIST_TAR) ## Build a release

untar-sdist: $(INTERNAL_SDIST_TAR)
	@mkdir -p tmp
	tar xzf $(INTERNAL_SDIST_TAR)
	@rm -rf tmp/$(SDIST_DIR_NAME) || true
	mv $(SDIST_DIR_NAME) tmp

test-sdist: untar-sdist
	cd tmp/$(SDIST_DIR_NAME) && $(NIGHTLY_STACK) init --force && $(NIGHTLY_STACK) build --test --bench --haddock --no-run-benchmarks

format: $(PROJECT_SETUP_FILE) ## Normalize style of source files
	$(FIND_HASKELL_FILES) -exec $(BRITTANY_FIND_EXEC) -exec $(STYLISH_FIND_EXEC) && git diff --exit-code

lint: $(PROJECT_SETUP_FILE) ## Execute linter
	$(FIND_HASKELL_FILES) -exec $(HLINT_FIND_EXEC) && git diff --exit-code

repl: $(PROJECT_SETUP_FILE) ## Start project's repl
	stack ghci

clean: ## Clean built artifacts
	rm -f $(PROJECT_BIN_DIR)/*
	rm -f target/*
	rm -rf tmp/*
	stack clean

dev-setup: $(PROJECT_SETUP_FILE) ## Install development dependencies

################################################################################
## Demo tasks

run-example1: $(EXAMPLE1_BIN) ## Runs example1 binary from tutorial
	$(EXAMPLE1_BIN) --procNumber 3

run-example2: $(EXAMPLE2_BIN) ## Runs example2 binary from tutorial
	$(EXAMPLE2_BIN) --procNumber 3
