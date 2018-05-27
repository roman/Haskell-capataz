# -*- mode: Makefile; -*-
################################################################################

CABAL_DIR ?= $$HOME/.local/bin
CABAL_BIN := $(CABAL_DIR)/cabal
STACK := stack $(STACK_ARGS) --install-ghc --local-bin-path ./out/bin

################################################################################

$(CABAL_BIN):
	stack --resolver lts-9 install --local-bin-path $(CABAL_DIR) cabal-install

cabal: $(CABAL_BIN) ## Install cabal for resolver
.PHONY: cabal

fix-solver: $(CABAL_BIN) ## Modifies stack.yaml to support dependency bounds
	$(STACK) --no-terminal test --bench --dry-run || \
		$(STACK) --no-terminal solver --update-config
.PHONY: fix-solver

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
