#!/bin/bash
set -euxo pipefail

bnfc -m CPP.cf # tested on BNFC version 2.9.4
make
ghc Main.hs -o lab3
