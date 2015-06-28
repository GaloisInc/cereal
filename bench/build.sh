#!/bin/sh

set -e

if [ ! -f cabal.sandbox.config ]; then
	cabal sandbox init
	cabal sandbox add-source ../
	cabal install --only-dep
	cabal configure
fi

cabal build
./dist/build/cereal-bench/cereal-bench
