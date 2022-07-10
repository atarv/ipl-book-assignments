#!/bin/bash
set -euxo pipefail

bnfc -m CPP.cf
make
ghc Main.hs -o lab2
