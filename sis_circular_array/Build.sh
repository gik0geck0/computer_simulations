#!/bin/sh

cabal configure
cabal build

mv ./dist/build/SIM/SIM .
