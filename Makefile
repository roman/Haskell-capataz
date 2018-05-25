################################################################################

STACK:=stack $(STACK_ARGS) --install-ghc --local-bin-path ./out/bin

HASKELL_FILES := $(shell find . -name "*.hs" -not -path '*.stack-work*' | grep 'src\|test')

PROJECT_BIN_DIR:=./out/bin
PROJECT_BIN=$(PROJECT_BIN_DIR)/teardown-example

STACK:=stack $(STACK_ARGS) --local-bin-path ./out/bin

################################################################################

$(PROJECT_BIN): $(HASKELL_FILES)
	$(STACK) build --copy-bins --local-bin-path $(PROJECT_BIN_DIR) --test --no-run-tests --bench --no-run-benchmarks --haddock --no-haddock-deps --pedantic

build: $(PROJECT_BIN)  ## Build library and example binaries
.PHONY: build

test: $(PROJECT_BIN) ## Execute test suites
	$(STACK) test --dump-logs
.PHONY: test

bench: $(PROJECT_BIN)
	$(STACK) bench --dump-logs
.PHONY: bench

clean: ## Clean built artifacts
	rm -f $(PROJECT_BIN_DIR)/*
	rm -f out/*
	rm -rf tmp/*
	stack clean
.PHONY: clean

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
