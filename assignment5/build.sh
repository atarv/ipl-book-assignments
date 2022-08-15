#!/bin/bash
set -eo pipefail

warn () {
    echo "$(tput setaf 3)""$*""$(tput sgr0)"
}

BNFC_VERSION=$(bnfc --version)
MINIMUM='2.9.4'
if [[ "$BNFC_VERSION" < "$MINIMUM" ]]; then
    warn "Warning: Minimum expected BNFC version is $MINIMUM, you have $BNFC_VERSION"
fi

bnfc -m Fun.cf && make && ghc Main.hs -o ifun
