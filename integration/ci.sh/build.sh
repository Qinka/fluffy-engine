#!/bin/bash
echo stop when error
set -e

echo build those
stack build --ghc-options -O2 --ghc-options -threaded
