#!/bin/bash
echo stop when error
set -e

echo
echo testing backend
stack test
