#!/bin/sh

set -e

cabal sandbox init
cabal sandbox add-source ../
cabal install --only-dep
cabal configure
cabal build
./dist/build/cereal-bench/cereal-bench
