#!/bin/bash
echo stop when error
set -e

echo
echo testing backend
stack test --ghc-options -O2 --ghc-options -threaded
