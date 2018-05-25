# -*- mode: Makefile; -*-
################################################################################

TOOLS_DIR ?= ./tools/bin

BRITTANY_BIN := $(TOOLS_DIR)/brittany
STYLISH_BIN := $(TOOLS_DIR)/stylish-haskell
HLINT_BIN := $(TOOLS_DIR)/hlint
PPSH_BIN := $(TOOLS_DIR)/ppsh
REFACTOR_BIN := $(TOOLS_DIR)/refactor

FIND_HASKELL_FILES := find . -name "*.hs" -not -path '*.stack-work*' -not -path "*tmp*" -not -name "Setup.hs"

STACK := stack --resolver lts-11 --local-bin-path $(TOOLS_DIR)

################################################################################

$(BRITTANY_BIN):
	$(STACK) install brittany

$(STYLISH_BIN):
	$(STACK) install stylish-haskell

$(REFACTOR_BIN):
	$(STACK) install apply-refact

$(HLINT_BIN):
	$(STACK) install hlint

################################################################################

format: $(STYLISH_BIN) $(BRITTANY_BIN) ## Normalize style on source files
	for f in $$($(FIND_HASKELL_FILES)); do echo $$f; $(BRITTANY_BIN) --config-file .brittany.yml --write-mode inplace $$f; $(STYLISH_BIN) -i $$f; done
	git diff --exit-code
.PHONY: format

remove-lint: $(HLINT_BIN) $(REFACTOR_BIN) ## Fix lint on source files automatically
	for f in $$($(FIND_HASKELL_FILES)); do echo $$f; $(HLINT_BIN) --hint=./.hlint.yml --with= --with-refactor=$(REFACTOR_BIN) --refactor --refactor-options -i $$f; done
	git diff --exit-code
.PHONY: remove-lint

lint: $(HLINT_BIN) ## Execute linter on source files
	for f in $$($(FIND_HASKELL_FILES)); do echo $$f; $(HLINT_BIN) --hint=./.hlint.yml --with= $$f; done
.PHONY: lint

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
