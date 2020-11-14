# this file assumes you're already in a nix-shell
#
# this file builds using setup commands (not cabal, not nix-build)

CABAL_FILE = z3.cabal
SETUP_CMD = runhaskell Setup.hs
CONFIG_FILE = dist/setup-config
# These LD flags are necessary to locate libz3.so on nixos
LDFLAGS = $(shell tr <<< "$$NIX_LDFLAGS" ' ' '\n' | sed 's|-L|--extra-lib-dirs=|;t;d')

.PHONY: test build clean repl ghcid entr-build entr-test

test: build
	$(SETUP_CMD) test

build: $(CONFIG_FILE)
	$(SETUP_CMD) build

$(CONFIG_FILE): $(CABAL_FILE)
	$(SETUP_CMD) configure --enable-tests $(LDFLAGS)

clean: $(CABAL_FILE)
	$(SETUP_CMD) clean

## tools

repl: $(CONFIG_FILE)
	$(SETUP_CMD) repl $(basename $(CABAL_FILE))

ghcid:
	ghcid -c make repl
entr-build:
	git ls-files | entr -c bash -c 'make build; echo done'
entr-test:
	git ls-files | entr -c bash -c 'make test; echo done'
