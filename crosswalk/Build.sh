#!/bin/sh

pushd haskell-lehmer
cabal configure
cabal build
popd

cabal configure
cabal build

mv ./dist/build/SIM/SIM .
